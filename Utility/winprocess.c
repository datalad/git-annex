#include <windows.h>

void terminatepid (DWORD pid) {
	HANDLE h;
	h = OpenProcess(PROCESS_TERMINATE, 0, pid);
	if (h != NULL) {
		TerminateProcess(h, 1);
	}
	CloseHandle(h);
}
