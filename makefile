VERSION_EL:=0.0.2
PACKAGE_NAME_EL:=sayid-$(VERSION_EL)
PACKAGE_DIR_EL:=/tmp/$(PACKAGE_NAME_EL)

package-el: $(PACKAGE_DIR_EL)
	tar cvf ../$(PACKAGE_NAME_EL).tar --exclude="*#" --exclude="*~" -C $(PACKAGE_DIR_EL)/.. $(PACKAGE_NAME_EL)

$(PACKAGE_DIR_EL):
	mkdir $@
	cp -r ./src/el/* $@

clean:
	rm -f ../$(PACKAGE_NAME_EL).tar
	rm -rf $(PACKAGE_DIR_EL)
