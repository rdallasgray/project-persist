PROJECT_LCNAME=project-persist
include el.mk/el.mk

.PHONY : test

test:
	@cask exec ert-runner
