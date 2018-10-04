FC = mpif90
AR = ar
LIB_A = libbcpl.a
ABSDIR = $(shell pwd)
SRC  = lib/*.o
LIBDIR = ./lib
path0 = MCTWrapper
path1 = data_def
path2 = timeManage
path3 = transManage
path4 = procManage
models=$(shell ls -l ./model|grep ^d|grep -v 'cpl'|awk '{print $$9}')
MAIN = ./model/cpl


.PHONY : all
all :
	#@for dir in $(models);\
	#do\
	#	cd ./model/$$dir;\
	#	make;\
	#	cd -;\
	#done
	$(AR) rcs $(LIB_A) $(SRC)
	mv *.a ./lib
	#rm ./lib/*.o

.PHONY : clean
clean :
	make clean -C $(path0)
	make clean -C $(path1)
	make clean -C $(path2)
	make clean -C $(path3)
	make clean -C $(path4)
	make clean -C $(model1)
	make clean -C $(model2)
	make clean -C $(model3)
	make clean -C $(model4)
	make clean -C $(model5)
	make clean -C $(model6)
	make clean -C $(MAIN)
	rm ./main
