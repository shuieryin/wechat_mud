all: dialyzer install ct

install:
	@./config/install.sh

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

ct: ct_run ct_analyze

ct_run:
	@./rebar3 ct
	@rm -f test/*.beam

ct_analyze:
	@./config/show_ct_errors.sh
	@./config/mail_test_result.sh

dialyzer:
	@./rebar3 dialyzer