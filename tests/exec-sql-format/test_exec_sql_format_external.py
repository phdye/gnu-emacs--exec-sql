#!/usr/bin/env python3
import os
import pty
import time

FILE_PATH = os.path.join('tests', 'examples', 'oracle+addtl.pc')
EXPECTED_FRAGMENT = (
    'SELECT id,\n'
    '       name INTO :v_id,\n'
    '                 :v_name\n'
    'FROM emp\n'
    'WHERE id = 10;'
)

def main():
    f = open(FILE_PATH, 'r')
    original = f.read()
    f.close()
    path = os.path.abspath(FILE_PATH)
    try:
        pid, fd = pty.fork()
        if pid == 0:
            emacs = 'emacs-nox'
            if os.system('which emacs-nox >/dev/null 2>&1') != 0:
                emacs = 'emacs'
            os.execvp(emacs, [emacs, '-Q', '-nw', '-L', '.', '-l', 'exec-sql-format.el', path])
        else:
            def send(data):
                os.write(fd, data)
                time.sleep(0.5)
            send(b'\x1bg' + b'g9\r')
            send(b'\x01')
            send(b'\x00')
            send(b'\x1bg' + b'g13\r')
            send(b'\x05')
            send(b'\x1bxexec-sql-format\r')
            time.sleep(1)
            send(b'\x18\x13')
            send(b'\x18\x03')
            os.waitpid(pid, 0)
            f = open(FILE_PATH, 'r')
            new_content = f.read()
            f.close()
            assert EXPECTED_FRAGMENT in new_content
            print('external format test passed')
    finally:
        f = open(FILE_PATH, 'w')
        f.write(original)
        f.close()

if __name__ == '__main__':
    main()
