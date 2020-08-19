all:
	rm -rf *@asus *.info configs  logfiles *_service include *~ */*~ */*/*~;
	rm -rf */*.beam;
	rm -rf *.beam erl_crash.dump */erl_crash.dump */*/erl_crash.dump;

	cp src/*.app ebin;
	erlc -o ebin src/*.erl;
doc_gen:
	rm -rf  node_config logfiles doc/*;
	erlc ../doc_gen.erl;
	erl -s doc_gen start -sname doc
test:
	rm -rf *@asus include configs *_service  erl_crasch.dump;
#	vm_service
#	git clone https://github.com/joq62/vm_service.git;	
	cp src/*.app ebin;
	erlc -o ebin src/*.erl;
#	test
	erlc -o test_ebin test_src/*.erl;
	erl -pa ebin -sname node1 -detached;
	erl -pa ebin -pa test_ebin -s dbase_service_tests start -sname dbase_test
