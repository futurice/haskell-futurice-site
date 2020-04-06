.PHONY : site

site :
	cabal new-build -w ghc-8.6.5
	$$(cabal-plan list-bin generate-site)
	@echo "file://`pwd`/site/index.html"

venv :
	virtualenv --python=python3.7 venv
	(. ./venv/bin/activate; pip install awscli)

pull :
	dotenv -f .env -- rsync -a '$$BUILDARTIFACTS' artifacts

push :
	(. ./venv/bin/activate; \
	aws s3 sync site \
	s3://haskell-futurice-com)
