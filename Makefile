.PHONY : site

site :
	cabal new-build
	`find dist-newstyle -type f -name generate-site`

venv :
	virtualenv --python=python3.5 venv
	(. ./venv/bin/activate; pip install awscli)

pull :
	dotenv -f .env -- rsync -a '$$BUILDARTIFACTS' artifacts

push :
	(. ./venv/bin/activate; \
	aws s3 --profile haskell.futurice.com sync site \
    s3://haskell.futurice.com)

