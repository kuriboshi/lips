test: ubuntu18 ubuntu20 ubuntu22 ubuntu22-clang

ubuntu18:
	docker build --build-arg USER_ID=$(shell id -u) --build-arg GROUP_ID=$(shell id -g) \
		-t 18 -f test/Ubuntu-18.04 .
	docker run --rm -ti -v $(shell pwd):/project 18 bash -c "test/run.sh build/18"

ubuntu20:
	docker build --build-arg USER_ID=$(shell id -u) --build-arg GROUP_ID=$(shell id -g) \
		-t 20 -f test/Ubuntu-20.04 .
	docker run --rm -ti -v $(shell pwd):/project 20 bash -c "test/run.sh build/20"

ubuntu22:
	docker build --build-arg USER_ID=$(shell id -u) --build-arg GROUP_ID=$(shell id -g) \
		-t 22 -f test/Ubuntu-22.04 .
	docker run --rm -ti -v $(shell pwd):/project 22 bash -c "test/run.sh build/22"

ubuntu22-clang:
	docker build --build-arg USER_ID=$(shell id -u) --build-arg GROUP_ID=$(shell id -g) \
		-t 22-clang -f test/Ubuntu-22.04-clang .
	docker run --rm -ti -v $(shell pwd):/project 22-clang bash -c "test/run.sh build/22-clang"
