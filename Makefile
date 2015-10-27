all:
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
