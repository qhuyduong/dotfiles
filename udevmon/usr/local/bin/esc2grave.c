#include <stdio.h>
#include <stdlib.h>
#include <linux/input.h>

int main(void) {
	setbuf(stdin, NULL), setbuf(stdout, NULL);

	struct input_event event;
	while (fread(&event, sizeof(event), 1, stdin) == 1) {
		if (event.type == EV_KEY && event.code == KEY_ESC) {
			event.code = KEY_GRAVE;
		} else if (event.type == EV_KEY && event.code == KEY_GRAVE) {
			event.code = KEY_ESC;
		}

		fwrite(&event, sizeof(event), 1, stdout);
	}
}

