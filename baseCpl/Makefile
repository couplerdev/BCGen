FC = mpif90
AR = ar
RM = rm
LIB_A = libbcpl.a
ABSDIR = $(shell pwd)
SRC  = lib/*.o
LIBDIR = ./lib
CCSM_SHR = ccsm_shr
DEFDIR = data_def
TIMEDIR= timeManage
COMMDIR = transManage
PROCDIR = procManage
MRGDIR = MrgSubroutine
UTILSDIR = utils
UTILAUXDIR = utilsAux
ESMDIR = esm
LOGUTILDIR =  logUtils 
FLUXDIR = FluxSubroutine
#models=$(shell ls -l ./src/model|grep ^d|grep -v 'cpl'|awk '{print $$9}')
MAIN = ./model/cpl
DEPS=$(CCSM_SHR)  $(LOGUTILDIR) $(DEFDIR) $(ESMDIR) $(UTILSDIR) $(TIMEDIR) $(COMMDIR) $(PROCDIR) $(MRGDIR) $(UTILAUXDIR) $(FLUXDIR)

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
	cp -f $(LIB_A) $(LIBDIR)
	#@for dir in $(models);\
	#do\
	#	echo $$dir; \
	#	cd src/model/$$dir; \
	#	make;\
	#	cd -;\
	#done
		#cd ./model/$$dir;\
		#make;\
		#cd -;\
	#done
	#$(AR) rcs $(LIB_A) $(SRC)
	#mv *.a ./lib
	#rm ./lib/*.o
	cp ./include/*.mod ../include/
	cp ./lib/*.o ../lib
	cp ./lib/*.a ../lib

.PHONY : clean
clean :
	$(RM) ./include/*.mod
	$(RM) ./lib/*.o 
	$(RM) ./lib/*.a
