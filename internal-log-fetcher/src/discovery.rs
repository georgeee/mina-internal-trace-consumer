// Copyright (c) Viable Systems
// SPDX-License-Identifier: Apache-2.0

use anyhow::Result;
use chrono::{DateTime, Utc};
use futures_util::StreamExt;
use object_store::aws::{AmazonS3, AmazonS3Builder};
use object_store::{path::Path, ObjectStore};
use serde::Deserialize;
use std::collections::HashSet;
use std::env;
use tracing::{info, warn};

use crate::node::NodeIdentity;

#[derive(Debug, Deserialize)]
struct MetaToBeSaved {
    remote_addr: String,
    submitter: String,
    graphql_control_port: u16,
}

struct AwsConfig {
    s3: AmazonS3,
    prefix: String,
}

pub struct DiscoveryService {
    online_url: Option<String>,
    aws: Option<AwsConfig>,
}

fn offset_by_time(prefix_str: String, t: DateTime<Utc>) -> String {
    let t_str = t.to_rfc3339_opts(chrono::SecondsFormat::Secs, true);
    let d_str = t.format("%Y-%m-%d");
    format!("{}/{}/{}", prefix_str, d_str, t_str)
}

fn new_from_aws() -> Result<DiscoveryService> {
    let prefix = env::var("AWS_PREFIX")?;
    let bucket = env::var("AWS_BUCKET")?;
    let s3 = AmazonS3Builder::from_env()
        .with_bucket_name(bucket)
        .build()?;
    let aws = AwsConfig { s3, prefix };
    Ok(DiscoveryService {
        aws: Some(aws),
        online_url: None,
    })
}

fn node_identity(
    remote_addr: &str,
    control_port: u16,
    submitter: String,
    url_overrides: Option<&[String]>,
) -> NodeIdentity {
    if control_port >= 10000 {
        let index = (control_port / 10000) as usize - 1;
        if let Some(overrides) = url_overrides {
            if let Some(url_template) = overrides.get(index) {
                let port_suffix = control_port % 10000;
                let ip = url_template.replace("{}", &port_suffix.to_string());
                return NodeIdentity {
                    ip,
                    graphql_port: 80,
                    submitter_pk: Some(submitter),
                };
            }
        }
    }
    NodeIdentity {
        ip: remote_addr.to_string(),
        graphql_port: control_port,
        submitter_pk: Some(submitter),
    }
}

pub async fn fetch_online(
    url: &str,
    url_overrides: Option<&[String]>,
) -> Result<HashSet<NodeIdentity>> {
    // Use "reqwest" to make an async GET request.
    let response = reqwest::get(url).await?;

    // Deserialize the JSON response into Vec<Meta>.
    let meta_array = response.json::<Vec<MetaToBeSaved>>().await?;

    let mut results = HashSet::new();
    for meta in meta_array {
        let node = node_identity(
            &meta.remote_addr,
            meta.graphql_control_port,
            meta.submitter.clone(),
            url_overrides,
        );
        results.insert(node);
    }

    Ok(results)
}

async fn discover_aws(aws: &AwsConfig) -> Result<HashSet<NodeIdentity>> {
    let before = Utc::now() - chrono::Duration::minutes(20);
    let prefix_str = format!("{}/submissions", aws.prefix);
    let prefix_str2 = prefix_str.clone();
    let offset: Path = offset_by_time(prefix_str, before).try_into()?;
    let prefix: Path = prefix_str2.into();
    info!("Obtaining list of objects in bucket...");
    let it = aws.s3.list_with_offset(Some(&prefix), &offset).await?;
    let mut results = HashSet::new();
    let list_results: Vec<_> = it.collect().await;
    info!("Results count {}", list_results.len());

    let list_results: Vec<_> = list_results
        .into_iter()
        .filter_map(|result| match result {
            Err(err) => {
                warn!("Got error when fetching listing objects: {:?}", err);
                None
            }
            Ok(result) => Some(result),
        })
        .rev()
        .collect();

    let futures = list_results.into_iter().map(|object_meta| async move {
        let bytes = aws
            .s3
            .get_range(&object_meta.location, 0..1_000_000_000)
            .await?;
        let meta: MetaToBeSaved = serde_json::from_slice(&bytes)?;
        Ok((object_meta.location, meta))
    });

    let aws_results: Vec<anyhow::Result<(Path, MetaToBeSaved)>> =
        futures_util::future::join_all(futures).await;
    let aws_results = aws_results.into_iter().filter_map(|result| match result {
        Err(err) => {
            warn!("Failure when fetching object: {:?}", err);
            None
        }
        Ok(result) => Some(result),
    });

    for (_, meta) in aws_results {
        let colon_ix = meta.remote_addr.find(':').unwrap_or(meta.remote_addr.len());
        results.insert(NodeIdentity {
            ip: meta.remote_addr[..colon_ix].to_string(),
            graphql_port: meta.graphql_control_port,
            submitter_pk: Some(meta.submitter),
        });
    }

    Ok(results)
}

impl DiscoveryService {
    pub fn try_new() -> Result<Self> {
        match env::var("ONLINE_URL") {
            Ok(url) if !url.is_empty() => Ok(DiscoveryService {
                online_url: Some(url),
                aws: None,
            }),
            _ => new_from_aws(),
        }
    }

    pub async fn discover_participants(
        &self,
        url_overrides: Option<Vec<String>>,
    ) -> Result<HashSet<NodeIdentity>> {
        match &self.aws {
            Some(aws) => discover_aws(aws).await,
            None => match &self.online_url {
                Some(url) => fetch_online(url, url_overrides.as_deref()).await,
                None => panic!("neither aws nor online url configured"),
            },
        }
    }
}
