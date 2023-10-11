DUNE=dune
OUTPUT=output
LIBNAME=libkosu.a

CONFIG=kosu_config.mk

KOSU_RUNTIME_OBJ=$(OUTPUT)/u8.o $(OUTPUT)/s8.o

all: kosuc kosu kosu_runtime man

configure:
	dune build lib/configure/configure.exe
	ln -sf _build/default/lib/configure/configure.exe configure


ifneq ($(shell test -f "$(CONFIG)"), 0)
	$(shell touch $(CONFIG))
else
	include kosu_config.mk
endif



okosuc:
	mkdir -p $(OUTPUT)
	$(DUNE) build bin/$@.exe
	cp -f _build/default/bin/$@.exe $(OUTPUT)/$@

kosuc:
	mkdir -p $(OUTPUT) 
	$(DUNE) build bin/$@.exe
	cp -f _build/default/bin/$@.exe $(OUTPUT)/$@

kosu:
	mkdir -p $(OUTPUT)
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
