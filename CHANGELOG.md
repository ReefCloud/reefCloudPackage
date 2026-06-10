# Changelog

All notable changes to the ReefCloud FRK Model application will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to Semantic Versioning (YYYY.MM.PATCH).

## [2024.11.0] - 2024-11-07

### Added
- Initial repository structure for ReefCloud FRK Model application
- Dockerfile using reefcloud-base-image from ECR
- Entrypoint script with comprehensive parameter handling
- Support for both S3 and local data paths
- Automatic data copying (input and output)
- Command-line parameter interface
- GitHub Actions workflow for automated ECR deployment
- Comprehensive documentation and examples
- reefCloudPackage with all performance optimizations

### Features
- **Dual data source support**: S3 buckets or local filesystem
- **Flexible parameters**: 9 configurable parameters via command-line
- **Automatic data management**: Copy input → process → export results
- **200GB ephemeral storage**: For large dataset processing
- **Fast builds**: 5-10 minute builds using pre-built base image
- **AWS Fargate ready**: Configured for serverless container execution

### Model Components
- FRK/INLA spatial-temporal modeling
- Multi-tier spatial hierarchy (Tier 2-5)
- Disturbance effect estimation
- Uncertainty quantification
- Hierarchical predictions

### Performance
- Vectorized covariate joins (100-500x faster)
- Optimized spatial operations
- Expected runtime: 2-4 hours for full GBR dataset
- Memory efficient: 120GB RAM recommended

### Infrastructure
- Base image: reefcloud-base-image:2024.11.0
- ECR repository: reefcloud-model-frk
- AIMS-compliant GitHub Actions
- OIDC authentication with AWS

## [Unreleased]

### Planned
- Additional model types (type3, type4)
- Real-time progress reporting
- Checkpoint/resume functionality
- Multi-region parallel processing
- Enhanced error reporting and recovery

---

## Version Format

Versions follow the format `YYYY.MM.PATCH`:
- `YYYY`: Year (4 digits)
- `MM`: Month (2 digits, 01-12)
- `PATCH`: Incremental patch number (0, 1, 2, ...)

## Change Categories

- **Added**: New features or functionality
- **Changed**: Changes to existing functionality
- **Deprecated**: Features that will be removed in future versions
- **Removed**: Removed features
- **Fixed**: Bug fixes
- **Security**: Security-related changes
- **Performance**: Performance improvements
