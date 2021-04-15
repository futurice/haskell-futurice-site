# haskell-futurice-site

To update site run 

```bash
make site
aws s3 sync site s3://haskell-futurice-com
```

Also invalidate cache at AWS CloudFront. You may also need build artifacts for cabal from build server
