INSTALL_DIR=/usr/local
INSTALL_BIN_DIR=$(INSTALL_DIR)/bin
INSTALL_LIB_DIR=$(INSTALL_DIR)/lib
INSTALL_HEADER_DIR=$(INSTALL_DIR)/include/kosu
INSTALL_MAN_DIR=$(INSTALL_DIR)/share/man
INSTALL_STD_DIR=$(INSTALL_DIR)/share/kosu/std
DUNE=dune

OUTPUT=output
LIBNAME=libkosu.a

KOSU_RUNTIME_OBJ=$(OUTPUT)/u8.o $(OUTPUT)/s8.o

BRANCH=$(shell git rev-parse --abbrev-ref HEAD)
COMMIT_HASH=$(shell git describe --always --dirty --abbrev=7)
# lowercase name
OS_NAME=$(shell uname -s | tr A-Z a-z)
OS_ARCH=$(shell uname -m)
OS_DYNLIB_EXE=$(shell \
	if [ "$$(uname)" = "Darwin" ]; then \
		echo .dylib; \
	else \
		echo .so; \
	fi \
)
OS_CC=cc
OS_AR=$(shell which ar)
KOSU_VERSION=$(cat kosu_lang.opam | grep ^version | awk '{print $2}')
LINKER_OPTS=""
LINKER_ARGS=""
ifeq ($(OS_NAME), darwin)
	LINKER_OPTS="syslibroot \`xcrun --sdk macosx --show-sdk-path\`","lSystem"
else ifeq ($(OS_NAME), freebsd)
	LINKER_OPTS="lc","dynamic-linker /libexec/ld-elf.so.1","L/usr/lib"
	LINKER_ARGS="/lib64/crt1.o", "/lib64/crti.o", "/lib64/crtn.o"
else ifeq ($(OS_NAME), linux)
	LINKER_OPTS="lc","dynamic-linker /lib64/ld-linux-x86-64.so.2","L/usr/lib L/lib64"
	LINKER_ARGS="/usr/lib/crt1.o","/usr/lib/crti.o","/usr/lib/crtbegin.o","/usr/lib/crtend.o","/usr/lib/crtn.o"
endif

all: kosuc kosu kosu_runtime man


okosuc:
	mkdir -p $(OUTPUT)
	$(DUNE) build lib/commandline/cliCommon/configure.exe 
	make kosuConfig
	$(DUNE) build bin/$@.exe
	cp -f _build/default/bin/$@.exe $(OUTPUT)/$@

kosuc:
	mkdir -p $(OUTPUT) 
	$(DUNE) build lib/commandline/cliCommon/configure.exe
	make kosuConfig
	$(DUNE) build bin/$@.exe
	cp -f _build/default/bin/$@.exe $(OUTPUT)/$@

kosu:
	mkdir -p $(OUTPUT)
	$(DUNE) build lib/commandline/cliCommon/configure.exe
	make kosuConfig
	$(DUNE) build bin/$@.exe
	cp -f _build/default/bin/$@.exe $(OUTPUT)/$@

install:
	mkdir -p "$(DESTDIR)$(INSTALL_BIN_DIR)"
	mkdir -p "$(DESTDIR)$(INSTALL_LIB_DIR)"
	mkdir -p "$(DESTDIR)$(INSTALL_HEADER_DIR)"
	mkdir -p "$(DESTDIR)$(INSTALL_MAN_DIR)"
	mkdir -p "$(DESTDIR)$(INSTALL_STD_DIR)"
	cp -f "$(OUTPUT)/kosuc" $(DESTDIR)$(INSTALL_BIN_DIR)
	cp -f "$(OUTPUT)/kosu" $(DESTDIR)$(INSTALL_BIN_DIR)
	cp -f "$(OUTPUT)/$(LIBNAME)" $(DESTDIR)$(INSTALL_LIB_DIR)
	cp -rf src/runtime/include/* $(INSTALL_HEADER_DIR)/
	cp -rf $(OUTPUT)/man/* "$(DESTDIR)$(INSTALL_MAN_DIR)"
	cp -rf src/std/* "$(DESTDIR)$(INSTALL_STD_DIR)"

uninstall:
	rm -f $(DESTDIR)$(INSTALL_BIN_DIR)/kosuc
	rm -f $(DESTDIR)$(INSTALL_BIN_DIR)/kosu
	rm -f $(DESTDIR)$(INSTALL_LIB_DIR)/$(LIBNAME)
	rm -rf $(INSTALL_HEADER_DIR)
	rm -r $(DESTDIR)$(INSTALL_MAN_DIR)/kosu*.1
	rm -r $(DESTDIR)$(INSTALL_STD_DIR)

man: kosuc kosu
	mkdir -p $(OUTPUT)/man
	_build/default/bin/kosuc.exe --help=groff > $(OUTPUT)/man/kosuc.1
	_build/default/bin/kosu.exe --help=groff > $(OUTPUT)/man/kosu.1
	_build/default/bin/kosu.exe cfg --help=groff > $(OUTPUT)/man/kosu-cfg.1
	_build/default/bin/kosu.exe repl --help=groff > $(OUTPUT)/man/kosu-repl.1


kosuConfig:
	_build/default/lib/commandline/cliCommon/configure.exe -a $(OS_ARCH) -o $(OS_NAME) --cc $(OS_CC) --os-extension $(OS_DYNLIB_EXE) -b $(BRANCH) \
		--hash $(COMMIT_HASH) -H $(INSTALL_HEADER_DIR) -c $(INSTALL_STD_DIR) -r $(INSTALL_LIB_DIR) --lo $(LINKER_OPTS) --lar $(LINKER_ARGS)\
		> lib/commandline/cliCommon/kosuConfig.ml


kosu_runtime:
		[ ! -d "$(OUTPUT)" ] && mkdir -p $(OUTPUT) || true
		make $(LIBNAME)

$(LIBNAME): $(KOSU_RUNTIME_OBJ)
	$(OS_AR) rcs $(OUTPUT)/$(LIBNAME) $^


$(OUTPUT)/%.o: src/runtime/src/%.c
	$(OS_CC) -fPIC -O2 -c -o $@ $^

# TODO linker option

.PHONY: test

build:
	dune build

san:
	dune build --profile san

clean:
	[ -d "$(OUTPUT)" ] && rm -rf output || true
	dune clean

# install:
# 	dune install

test:
	dune test
