PROJECT_VERSION ?= 3.10.0

_rel/RabbitMQ/RabbitMQ-$(PROJECT_VERSION).tar.gz:
	docker run --interactive --tty --rm \
		--volume $(CURDIR):/workspace \
		--workdir /workspace \
		pivotalrabbitmq/rabbitmq-server-buildenv:linux-erlang-24.0-elixir-latest \
		make PROJECT_VERSION=$(PROJECT_VERSION)
.PHONY: release
release: _rel/RabbitMQ/RabbitMQ-$(PROJECT_VERSION).tar.gz

.PHONY: container-image
container-image: container-image-build container-image-push

.PHONY: container-image-build
container-image-build:
	docker build --progress plain \
		--build-arg RABBITMQ_VERSION=$(PROJECT_VERSION) \
		--tag pivotalrabbitmq/rabbitmq:erlang-release-$(PROJECT_VERSION) \
		.

.PHONY: container-image-push
container-image-push:
	docker push pivotalrabbitmq/rabbitmq:erlang-release-$(PROJECT_VERSION)

.PHONY: container
container:
	docker run --interactive --tty --rm \
		-p 15672:15672 \
		pivotalrabbitmq/rabbitmq:erlang-release-$(PROJECT_VERSION) \
		$(CMD)

.PHONY: clean
clean:
	rm -fr _rel
