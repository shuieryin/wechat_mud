all: check install ct

install:
	@./config/install.sh

check: dialyzer edoc

cu: clean_upgrade

clean_upgrade: remove_appup upgrade

remove_appup:
	@rm -f ebin/*.appup

upgrade:
	@#bash -x
	@./config/hcu.sh

run:
	@./config/run.sh

reset:
	@git fetch --all
	@git reset --hard origin/master

ct:
	@./config/rebar3 do ct -c, cover
	@rm -f test/*.beam
	@./config/test_coverage.sh

ct_analyze:
	@./config/show_ct_errors.sh

dialyzer:
	@./config/rebar3 dialyzer

edoc:
	@./config/gen_edoc.sh