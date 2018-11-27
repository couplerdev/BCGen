FC = mpif90
include ./Makefile.conf
models=$(shell ls -l ./models | grep ^d|grep -v 'cpl'|awk '{print $$9}')

.PHONY : all
all :
	@for dir in $(models);\
	do\
		echo $$dir; \
		cd ./models/$$dir; \
		make; \
		cd -; \
	done
	


