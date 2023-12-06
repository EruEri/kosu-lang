DUNE=dune
OUTPUT=output
OS_AR=$(shell which ar)
OS_CC=$(shell which cc)
CONFIG=kosu_config.mk

-include $(CONFIG)

KOSU_RUNTIME_OBJ=$(OUTPUT)/u8.o $(OUTPUT)/s8.o

all: kosuc okosuc kosu kosu_runtime man

configure: lib/configure/configure.ml
	dune build lib/configure/configure.exe
	ln -sf _build/default/lib/configure/configure.exe configure

ifneq ($(shell test -f "$(CONFIG)"), 0)
	$(shell touch $(CONFIG))
endif

okosuc :
	$(DUNE) build bin/$@.exe
	mkdir -p $(OUTPUT)
	cp -f _build/default/bin/okosuc.exe $(OUTPUT)/$@

kosuc:
	$(DUNE) build bin/$@.exe
	mkdir -p $(OUTPUT) 
	cp -f _build/default/bin/$@.exe $(OUTPUT)/$@

kosu:
	$(DUNE) build bin/$@.exe
	mkdir -p $(OUTPUT)
	cp -f _build/default/bin/$@.exe $(OUTPUT)/$@

install:
	mkdir -p $(DESTDIR)$(INSTALL_BIN_DIR)
	mkdir -p $(DESTDIR)$(INSTALL_LIB_DIR)
	mkdir -p $(DESTDIR)$(INSTALL_HEADER_DIR)
	mkdir -p $(DESTDIR)$(INSTALL_MAN_DIR)/man1
	mkdir -p $(DESTDIR)$(INSTALL_STD_DIR)
	cp -f "$(OUTPUT)/kosuc" $(DESTDIR)$(INSTALL_BIN_DIR)
	cp -f "$(OUTPUT)/kosu" $(DESTDIR)$(INSTALL_BIN_DIR)
	cp -f $(OUTPUT)/okosuc $(DESTDIR)$(INSTALL_BIN_DIR)
	cp -f "$(OUTPUT)/libkosu.$(KOSU_VERSION).a" $(DESTDIR)$(INSTALL_LIB_DIR)
	cp -rf src/runtime/include/* $(INSTALL_HEADER_DIR)/
	cp -rf $(OUTPUT)/man/*.1 "$(DESTDIR)$(INSTALL_MAN_DIR)/man1"
	cp -rf src/std/* "$(DESTDIR)$(INSTALL_STD_DIR)"

uninstall:
	rm -f $(DESTDIR)$(INSTALL_BIN_DIR)/kosuc
	rm -f $(DESTDIR)$(INSTALL_BIN_DIR)/kosu
	rm -f $(DESTDIR)$(INSTALL_BIN_DIR)/okosuc
	rm -f $(DESTDIR)$(INSTALL_LIB_DIR)/libkosu.$(KOSU_VERSION).a
	rm -rf $(INSTALL_HEADER_DIR)
	rm -r $(DESTDIR)$(INSTALL_MAN_DIR)/*kosu*.1
	rm -r $(DESTDIR)$(INSTALL_STD_DIR)

man: kosuc kosu okosuc
	mkdir -p $(OUTPUT)/man
	_build/default/bin/okosuc.exe --help=groff > $(OUTPUT)/man/okosuc.1
	_build/default/bin/kosuc.exe --help=groff > $(OUTPUT)/man/kosuc.1
	_build/default/bin/kosu.exe --help=groff > $(OUTPUT)/man/kosu.1
	_build/default/bin/kosu.exe cfg --help=groff > $(OUTPUT)/man/kosu-cfg.1
	_build/default/bin/kosu.exe repl --help=groff > $(OUTPUT)/man/kosu-repl.1


kosuConfig:
	_build/default/lib/commandline/cliCommon/configure.exe \
	> lib/commandline/cliCommon/kosuConfig.ml


kosu_runtime: $(KOSU_RUNTIME_OBJ)
	mkdir -p $(OUTPUT)
	$(OS_AR) rcs $(OUTPUT)/libkosu.$(KOSU_VERSION).a $^


$(OUTPUT)/%.o: src/runtime/src/%.c
	$(OS_CC) -fPIC -O2 -c -o $@ $^

# TODO linker option

.PHONY: test

build:
	dune build

san:
	dune build --profile san

clean:
	test -d "$(OUTPUT)" && rm -rf $(OUTPUT) || true
	test -f configure && rm configure || true
	test -f $(CONFIG) && rm $(CONFIG) || true
	dune clean

# install:
# 	dune install

test:
	dune test
