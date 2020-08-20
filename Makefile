all:
	rm -rf *@asus *.info configs  logfiles *_service include *~ */*~ */*/*~;
	rm -rf */*.beam;
	rm -rf *.beam erl_crash.dump */erl_crash.dump */*/erl_crash.dump;
#	include
	git clone https://github.com/joq62/include.git;
	cp src/*.app ebin;
	erlc -I include -o ebin src/*.erl;
doc_gen:
	rm -rf  node_config logfiles doc/*;
	erlc ../doc_gen.erl;
	erl -s doc_gen start -sname doc
test:
	rm -rf *@asus include configs *_service  erl_crasch.dump;
#	include
	git clone https://github.com/joq62/include.git;	
	cp src/*.app ebin;
	erlc -I include -o ebin src/*.erl;
#	test
	erlc -I include -o test_ebin test_src/*.erl;
	erl -pa ebin -sname node1 -detached;
	erl -pa ebin -pa test_ebin -s dbase_service_tests start -sname dbase_test
