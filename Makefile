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

ct:
	@./config/rebar3 ct
	@rm -f test/*.beam

ct_analyze:
	@./config/show_ct_errors.sh
	@./config/mail_test_result.sh

dialyzer:
	@./config/rebar3 dialyzer