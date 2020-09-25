---
title: "Continuous Integration and delivery on Gitlab CI, testing locally with Docker"
date: 2017-12-01T14:21:20+01:00
tags: ["tutorial", "testing",]
draft: false
---

*Best read in the [Cookbook](https://lispcookbook.github.io/cl-cookbook/testing.html#gitlab-ci) !* also Travis CI, code coverage, testing with Prove.


[Gitlab CI](https://docs.gitlab.com/ce/ci/README.html) is part of
Gitlab and is available on [Gitlab.com](https://gitlab.com/), for
public and private repositories. Let's see straight away a simple
`.gitlab-ci.yml`:

~~~
image: daewok/lisp-devel

before_script:
  - apt-get update -qy
  - apt-get install -y git-core
  - git clone https://github.com/foo/bar ~/quicklisp/local-projects/

test:
  script:
    - make test
~~~

Gitlab CI is based on Docker. With `image` we tell it to use the
[daewok/lisp-devel](https://hub.docker.com/r/daewok/lisp-devel/)
one. It includes SBCL, ECL, CCL and ABCL, and Quicklisp is installed
in the home (`/home/lisp/`), so we can `quickload` packages right
away. If you're interested it also has a more bare bones option. Gitlab will load the
image, clone our project and put us at the project root with
administrative rights to run the rest of the commands.

`test` is a "job" we define, `script` is a
recognized keywords that takes a list of commands to run.

Suppose we must install dependencies before running our tests:
`before_script` will run before each job. Here we clone a library
where Quicklisp can find it, and for doing so we must install git
(Docker images are usually pretty bare bones).

We can try locally ourselves. If we already installed [Docker](https://docs.docker.com/) and
started its daemon (`sudo service docker start`), we can do:

    docker run --rm -it -v /path/to/local/code:/usr/local/share/common-lisp/source daewok/lisp-devel:latest bash

This will download the lisp image (±400Mo), mount some local code in
the image where indicated, and drop us in bash. Now we can try a `make
test`.

To show you a more complete example:

~~~
image: daewok/lisp-devel

stages:
  - test
  - build

before_script:
  - apt-get update -qy
  - apt-get install -y git-core
  - git clone https://github.com/foo/bar ~/quicklisp/local-projects/

test:
  stage: test
  script:
    - make test

build:
  stage: build
  only:
    - tags
  script:
    - make build
  artifacts:
    paths:
      - some-file-name
~~~

Here we defined two `stages` (see
[environments](https://docs.gitlab.com/ce/ci/environments.html)),
"test" and "build", defined to run one after another. A "build" stage
will start only if the "test" one succeesds.

"build" is asked to run `only` when a
new tag is pushed, not at every commit. When it succeeds, it will make
the files listed in `artifacts`'s `paths` available for download. We can
download them from Gitlab's Pipelines UI, or with an url. This one will download
the file "some-file-name" from the latest "build" job:

    https://gitlab.com/username/project-name/-/jobs/artifacts/master/raw/some-file-name?job=build

When the pipelines pass, you will see:

![](https://lispcookbook.github.io/cl-cookbook/assets/img-ci-build.png)


You now have a ready to use Gitlab CI.
