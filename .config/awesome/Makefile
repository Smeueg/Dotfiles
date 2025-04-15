test:
	@{ \
    display=1;\
    while [  "$(pgrep -f "Xorg :${display}")" ]; do\
        display=$$((display + 1));\
    done;\
	Xephyr :$${display} -resizeable &\
	DISPLAY=:$${display} awesome; \
	}

.PHONY: test
