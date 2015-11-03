REBAR=./rebar3
PROJECT=memebot
TAG := $(shell git describe --tags)

AWS_ACCESS_KEY_ID := $(shell aws configure get aws_access_key_id)
AWS_SECRET_ACCESS_KEY := $(shell aws configure get aws_secret_access_key)
AWS_REGION := $(shell aws configure get region)

project_dir := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

.PHONY: test

all:
	@${REBAR} compile

test:
	@${REBAR} eunit

clean:
	@${REBAR} clean

release:
	@${REBAR} release

build:
	docker build --tag $(PROJECT):$(TAG) .

build-release: build
	docker run --volume=$(project_dir):/data/ \
		--rm=true \
		--entrypoint=/bin/bash \
		$(PROJECT):$(TAG) \
		./build_release.sh
run:
	docker run --volume=$(project_dir):/data/ \
		--detach=true \
		--env=AWS_ACCESS_KEY_ID=$(AWS_ACCESS_KEY_ID) \
		--env=AWS_SECRET_ACCESS_KEY=$(AWS_SECRET_ACCESS_KEY) \
		--env=AWS_REGION=$(AWS_REGION) \
		--env=SLACK_CLIENT_ID=$(SLACK_CLIENT_ID) \
		--env=SLACK_CLIENT_SECRET=$(SLACK_CLIENT_SECRET) \
		--publish=8080:8080 \
		$(PROJECT):$(TAG) \
		foreground
