DOOM_EMACS_DIR = ~/.emacs.d

default:
	$(MAKE) -C $(DOOM_EMACS_DIR)

clean:
	$(MAKE) -C $(DOOM_EMACS_DIR) clean

complie:
	$(MAKE) -C $(DOOM_EMACS_DIR) compile

update:
	$(MAKE) -C $(DOOM_EMACS_DIR) update
