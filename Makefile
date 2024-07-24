USER_SHELL=$(shell echo $$SHELL | awk -F/ '{print $$NF}')

build:
	cargo build
	cp target/debug/narval /usr/local/bin/narval
	
install:
	mkdir -p /var/lib/narval/
	cp -r libs/ /var/lib/narval

	@if [ -z "$$NARVAL_HOME" ]; then \
		case $(USER_SHELL) in \
			bash) \
				printf "\nexport NARVAL_HOME=/var/lib/narval" >> ~/.bashrc; \
				. ~/.bashrc; \
				echo "Added NARVAL_HOME to bashrc"; \
				;; \
			zsh) \
				printf "\nexport NARVAL_HOME=/var/lib/narval" >> ~/.zshrc; \
				. ~/.zshrc; \
				echo "Added NARVAL_HOME to zshrc"; \
				;; \
			*) echo "Unknown shell: $(USER_SHELL). Please export NARVAL_HOME as /var/lib/narval";; \
		esac \
	fi