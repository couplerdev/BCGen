FC = mpif90
AR = ar
LIB_A = libbcpl.a
ABSDIR = $(shell pwd)
SRC  = lib/*.o
LIBDIR = ./lib
MCTMODDIR = MCTWrapper
DEFDIR = data_def
TIMEDIR= timeManage
COMMDIR = transManage
PROCDIR = procManage
MRGDIR = MrgSubroutine
SYSDIR = sys
ESM = esm
LOGUTILDIR =  logUtils 
models=$(shell ls -l ./src/model|grep ^d|grep -v 'cpl'|awk '{print $$9}')
MAIN = ./model/cpl
DEPS=$(MCTMODDIR) $(SYSDIR) $(LOGUTILDIR) $(DEFDIR) $(ESM) $(TIMEDIR) $(COMMDIR) $(PROCDIR) $(MRGDIR) 

.PHONY : all
all :
	@for dir in $(DEPS);\
	do\
		echo $$dir;\
		cd src/$$dir;\
		make; \
		cd -; \
	done
	$(AR) rcs $(LIB_A) ./lib/*.o
	mv $(LIB_A) $(LIBDIR)
	@for dir in $(models);\
	do\
		echo $$dir; \
		cd src/model/$$dir; \
		make;\
		cd -;\
	done
		#cd ./model/$$dir;\
		#make;\
		#cd -;\
	#done
	#$(AR) rcs $(LIB_A) $(SRC)
	#mv *.a ./lib
	#rm ./lib/*.o

.PHONY : clean
clean :
	make clean -C include
	make clean -C lib
	make clean -C $(MAIN)
	rm ./main
