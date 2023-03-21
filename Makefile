CFLAGS = -O2
HFLAGS = -O

all : reform$(EXE)

reform$(EXE) : Reform*.hs MD5.lhs md5_c.o
	ghc --make -fglasgow-exts -o $@ ${HFLAGS} Reform.hs md5_c.o
	strip $@

md5_c.o : md5_c.c md5_c.h
	gcc -c ${CFLAGS} $<

clean:
	rm -f *.o *.hi reform.exe
