VERSION:=0.1.0
PACKAGE_NAME:=sayid-$(VERSION)
PACKAGE_DIR:=/tmp/$(PACKAGE_NAME)

package-el:
	mkdir $(PACKAGE_DIR)
	cp -r src/el/* $(PACKAGE_DIR)
	tar cvf ../$(PACKAGE_NAME).tar --exclude="*#" --exclude="*~" -C $(PACKAGE_DIR)/.. $(PACKAGE_NAME)
	rm -rf $(PACKAGE_DIR)

# end
