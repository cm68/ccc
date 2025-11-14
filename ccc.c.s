; Function: usage(_:_void_) -> _void_
; Variable lifetimes:
;   _: unused (0 refs)
_usage:
	ld a, 0
	call framealloc
	; load address of $_printf
	; load address of $_progname
	; load word from address
	; load address of $_str0
	; op @ (0x40) size=2
	; load address of $_printf
	; load address of $_str1
	; op @ (0x40) size=2
	; load address of $_printf
	; load address of $_str2
	; op @ (0x40) size=2
	; load address of $_printf
	; load address of $_str3
	; op @ (0x40) size=2
	; load address of $_printf
	; load address of $_str4
	; op @ (0x40) size=2
	; load address of $_printf
	; load address of $_str5
	; op @ (0x40) size=2
	; load address of $_printf
	; load address of $_str6
	; op @ (0x40) size=2
	; load address of $_printf
	; load address of $_str7
	; op @ (0x40) size=2
	; load address of $_printf
	; load address of $_str8
	; op @ (0x40) size=2
	; load address of $_exit
	ld hl, 1
	; op @ (0x40) size=2
	call framefree
; Function: get_script_dir(argv0) -> :array:1024
; Variable lifetimes:
;   resolved: labels 0-0 (2 refs)
;   dir: labels 0-0 (2 refs)
;   path: labels 0-0 (3 refs)
;   argv0: labels 0-0 (2 refs)
_get_script_dir:
	ld a, 6
	call framealloc
	; load address of $_realpath
	; load address of $Aargv0
	; load address of $resolved
	; load word from address
	; op @ (0x40) size=2
	; load address of $path
	; load address of $_strdup
	; load address of $resolved
	; op @ (0x40) size=2
	; store word to address
	; load address of $path
	; load address of $_strdup
	; load address of $Aargv0
	; load word from address
	; op @ (0x40) size=2
	; store word to address
_if_end_1:
	; load address of $dir
	; load address of $_dirname
	; load address of $path
	; load word from address
	; op @ (0x40) size=2
	; store word to address
	; load address of $_strdup
	; load address of $dir
	; load word from address
	; op @ (0x40) size=2
	call framefree
; Function: make_temp_ast(basename) -> :ptr
; Variable lifetimes:
;   result: labels 2-2 (2 refs)
;   fd: labels 0-2 (3 refs)
;   template: labels 0-2 (3 refs)
;   basename: labels 0-0 (1 refs)
_make_temp_ast:
	ld a, 6
	call framealloc
	; load address of $_snprintf
	; load address of $Abasename
	; load word from address
	; load address of $_str9
	ld hl, 256
	; load address of $template
	; op @ (0x40) size=2
	; load address of $fd
	; load address of $_mkstemp
	; load address of $template
	; op @ (0x40) size=2
	; store word to address
	; load address of $fd
	; load word from address
	; op X (0x58) size=4
	ld hl, 0
	call lt1616
	; load address of $_perror
	; load address of $_str10
	; op @ (0x40) size=2
	; load address of $_exit
	ld hl, 1
	; op @ (0x40) size=2
_if_2:
	; load address of $_close
	; load address of $fd
	; load word from address
	; op @ (0x40) size=2
	; load address of $result
	; load address of $_strdup
	; load address of $template
	; op @ (0x40) size=2
	; store word to address
	; load address of $result
	; load word from address
	call framefree
; Function: get_basename_no_ext(filename) -> :ptr
; Variable lifetimes:
;   dot: labels 0-3 (4 refs)
;   result: labels 0-3 (3 refs)
;   base: labels 0-0 (2 refs)
;   temp: labels 0-0 (3 refs)
;   result: unused (0 refs)
;   fd: unused (0 refs)
;   template: unused (0 refs)
;   basename: labels 0-0 (1 refs)
;   filename: labels 0-0 (1 refs)
_get_basename_no_ext:
	ld a, 16
	call framealloc
	; load address of $temp
	; load address of $_strdup
	; load address of $Afilename
	; load word from address
	; op @ (0x40) size=2
	; store word to address
	; load address of $base
	; load address of $Abasename
	; load word from address
	; load address of $temp
	; load word from address
	; op @ (0x40) size=2
	; store word to address
	; load address of $result
	; load address of $_strdup
	; load address of $base
	; load word from address
	; op @ (0x40) size=2
	; store word to address
	; load address of $_free
	; load address of $temp
	; load word from address
	; op @ (0x40) size=2
	; load address of $dot
	; load address of $_strrchr
	; load address of $result
	ld hl, 46
	; load word from address
	; op @ (0x40) size=2
	; store word to address
	; load address of $dot
	; load word from address
	; load address of $_strcmp
	; load address of $dot
	; load address of $_str11
	; load word from address
	; op @ (0x40) size=2
	ld hl, 0
	call eq1616
	; op j (0x6a) size=2
	; load address of $dot
	; load word from address
	ld hl, 0
	; op N (0x4e) size=1
	; store byte to address
_if_3:
	; load address of $result
	; load word from address
	call framefree
; Function: exec_command(args, cmd) -> _short_
; Variable lifetimes:
;   status: labels 7-8 (3 refs)
;   pid: labels 0-7 (4 refs)
;   dot: unused (0 refs)
;   result: unused (0 refs)
;   base: unused (0 refs)
;   temp: unused (0 refs)
;   result: unused (0 refs)
;   fd: unused (0 refs)
;   template: unused (0 refs)
;   filename: unused (0 refs)
;   basename: unused (0 refs)
;   cmd: labels 6-6 (2 refs)
;   args: labels 6-6 (1 refs)
_exec_command:
	ld a, 22
	call framealloc
	; load address of $pid
	; load address of $_fork
	; op @ (0x40) size=2
	; store word to address
	; load address of $pid
	; load word from address
	; op X (0x58) size=4
	ld hl, 0
	call lt1616
	; load address of $_perror
	; load address of $_str12
	; op @ (0x40) size=2
	; load address of $_exit
	ld hl, 1
	; op @ (0x40) size=2
_if_5:
	; load address of $pid
	; load word from address
	; op X (0x58) size=4
	ld hl, 0
	call eq1616
	; load address of $_execv
	; load address of $Acmd
	; load address of $Aargs
	; load word from address
	; load word from address
	; op @ (0x40) size=2
	; load address of $_perror
	; load address of $Acmd
	; load word from address
	; op @ (0x40) size=2
	; load address of $_exit
	ld hl, 1
	; op @ (0x40) size=2
_if_6:
	; load address of $_waitpid
	; load address of $pid
	ld hl, 0
	; load address of $status
	; load word from address
	; op @ (0x40) size=2
	ld hl, 0
	call lt1616
	; load address of $_perror
	; load address of $_str13
	; op @ (0x40) size=2
	; load address of $_exit
	ld hl, 1
	; op @ (0x40) size=2
_if_7:
	; load address of $status
	; load word from address
	; op X (0x58) size=4
	ld hl, 127
	call and1616
	ld hl, 0
	call eq3216
	; load address of $status
	; load word from address
	; op X (0x58) size=4
	ld hl, 8
	call shr1616
	ld hl, 255
	call and3216
	ld hl, 1
_if_end_9:
	call framefree
; Function: main(argv, argc:_short_) -> _short_
; Variable lifetimes:
;   interp_argc: labels 45-45 (6 refs)
;   interp_args: labels 45-45 (6 refs)
;   interp_argc: unused (0 refs)
;   interp_args: unused (0 refs)
;   interp_argc: unused (0 refs)
;   interp_args: unused (0 refs)
;   interp_argc: unused (0 refs)
;   interp_args: unused (0 refs)
;   status: labels 41-47 (10 refs)
;   basename_no_ext: labels 40-48 (6 refs)
;   interp_path: labels 0-45 (2 refs)
;   cc2_path: labels 0-45 (3 refs)
;   cc1_path: labels 0-41 (3 refs)
;   cc2_argc: labels 0-45 (6 refs)
;   cc1_argc: labels 0-41 (13 refs)
;   cc2_args: labels 0-45 (6 refs)
;   cc1_args: labels 0-41 (10 refs)
;   execute_ast: labels 0-45 (3 refs)
;   keep_ast: labels 0-48 (7 refs)
;   ast_file: labels 0-48 (15 refs)
;   output_file: labels 0-48 (6 refs)
;   source_file: labels 0-41 (9 refs)
;   status: unused (0 refs)
;   pid: unused (0 refs)
;   dot: unused (0 refs)
;   result: unused (0 refs)
;   base: unused (0 refs)
;   temp: unused (0 refs)
;   result: unused (0 refs)
;   fd: unused (0 refs)
;   template: unused (0 refs)
;   cmd: unused (0 refs)
;   args: unused (0 refs)
;   filename: unused (0 refs)
;   basename: unused (0 refs)
;   argc: labels 0-36 (13 refs)
;   argv: labels 0-36 (30 refs)
_main:
	ld a, 70
	call framealloc
	; load address of $source_file
	ld hl, 0
	; op N (0x4e) size=2 unsigned
	; store word to address
	; load address of $output_file
	ld hl, 0
	; op N (0x4e) size=2 unsigned
	; store word to address
	; load address of $ast_file
	ld hl, 0
	; op N (0x4e) size=2 unsigned
	; store word to address
	; load address of $keep_ast
	ld hl, 0
	; store word to address
	; load address of $execute_ast
	ld hl, 0
	; store word to address
	; load address of $cc1_argc
	ld hl, 0
	; store word to address
	; load address of $cc2_argc
	ld hl, 0
	; store word to address
	; load address of $_progname
	; load address of $Aargv
	ld hl, 0
	call add1616
	; load word from address
	; store word to address
	; load address of $_scriptdir
	; load address of $_get_script_dir
	; load address of $Aargv
	ld hl, 0
	call add1616
	; load word from address
	; op @ (0x40) size=2
	; store word to address
	; load address of $_snprintf
	; load address of $_scriptdir
	; load word from address
	; load address of $_str14
	ld hl, 1024
	; load address of $cc1_path
	; op @ (0x40) size=2
	; load address of $_snprintf
	; load address of $_scriptdir
	; load word from address
	; load address of $_str15
	ld hl, 1024
	; load address of $cc2_path
	; op @ (0x40) size=2
	; load address of $_snprintf
	; load address of $_scriptdir
	; load word from address
	; load address of $_str16
	ld hl, 1024
	; load address of $interp_path
	; op @ (0x40) size=2
	; load address of $cc1_args
	; load address of $cc1_argc
	; op ? (0xef) size=2
	call add1616
	; load address of $cc1_path
	; store byte to address
	; load address of $cc2_args
	; load address of $cc2_argc
	; op ? (0xef) size=2
	call add1616
	; load address of $cc2_path
	; store byte to address
	; load address of $Aargc
	; op ? (0xf6) size=2
	; load address of $Aargv
	; op ? (0xef) size=2
	; load address of $Aargc
	; load word from address
	; op X (0x58) size=4
	ld hl, 0
	call gt1616
	; load address of $_strcmp
	; load address of $Aargv
	ld hl, 0
	call add1616
	; load address of $_str17
	; load word from address
	; op @ (0x40) size=2
	ld hl, 0
	call eq1616
	; load address of $_strcmp
	; load address of $Aargv
	ld hl, 0
	call add1616
	; load address of $_str18
	; load word from address
	; op @ (0x40) size=2
	ld hl, 0
	call eq1616
	; op h (0x68) size=2
	; load address of $_usage
	; op @ (0x40) size=2
	; load address of $_strcmp
	; load address of $Aargv
	ld hl, 0
	call add1616
	; load address of $_str19
	; load word from address
	; op @ (0x40) size=2
	ld hl, 0
	call eq1616
	; load address of $Aargc
	; op ? (0xf6) size=2
	; load address of $Aargv
	; op ? (0xef) size=2
	; load address of $Aargc
	; load word from address
	; op X (0x58) size=4
	ld hl, 0
	call eq1616
	; load address of $_fprintf
	; load address of $_stderr
	; load address of $_str20
	; load word from address
	; op @ (0x40) size=2
	; load address of $_usage
	; op @ (0x40) size=2
_if_15:
	; load address of $output_file
	; load address of $Aargv
	ld hl, 0
	call add1616
	; load word from address
	; store word to address
	; load address of $Aargc
	; op ? (0xf6) size=2
	; load address of $Aargv
	; op ? (0xef) size=2
	; load address of $_strcmp
	; load address of $Aargv
	ld hl, 0
	call add1616
	; load address of $_str21
	; load word from address
	; op @ (0x40) size=2
	ld hl, 0
	call eq1616
	; load address of $keep_ast
	ld hl, 1
	; op N (0x4e) size=2
	; store word to address
	; load address of $Aargc
	; op ? (0xf6) size=2
	; load address of $Aargv
	; op ? (0xef) size=2
	; load address of $_strcmp
	; load address of $Aargv
	ld hl, 0
	call add1616
	; load address of $_str22
	; load word from address
	; op @ (0x40) size=2
	ld hl, 0
	call eq1616
	; load address of $execute_ast
	ld hl, 1
	; op N (0x4e) size=2
	; store word to address
	; load address of $keep_ast
	ld hl, 1
	; op N (0x4e) size=2
	; store word to address
	; load address of $Aargc
	; op ? (0xf6) size=2
	; load address of $Aargv
	; op ? (0xef) size=2
	; load address of $_strcmp
	; load address of $Aargv
	ld hl, 0
	call add1616
	; load address of $_str23
	; load word from address
	; op @ (0x40) size=2
	ld hl, 0
	call eq1616
	; load address of $cc1_argc
	; load word from address
	; op X (0x58) size=4
	ld hl, 2
	call add1616
	ld hl, 2560
	call ge3216
	; load address of $_fprintf
	; load address of $_stderr
	; load address of $_str24
	; load word from address
	; op @ (0x40) size=2
	; load address of $_exit
	ld hl, 1
	; op @ (0x40) size=2
_if_22:
	; load address of $cc1_args
	; load address of $cc1_argc
	; op ? (0xef) size=2
	call add1616
	; load address of $_str25
	; store byte to address
	; load address of $Aargc
	; op ? (0xf6) size=2
	; load address of $Aargv
	; op ? (0xef) size=2
	; load address of $Aargc
	; load word from address
	; op X (0x58) size=4
	ld hl, 0
	call eq1616
	; load address of $_fprintf
	; load address of $_stderr
	; load address of $_str26
	; load word from address
	; op @ (0x40) size=2
	; load address of $_usage
	; op @ (0x40) size=2
_if_23:
	; load address of $cc1_args
	; load address of $cc1_argc
	; op ? (0xef) size=2
	call add1616
	; load address of $Aargv
	ld hl, 0
	call add1616
	; load word from address
	; store byte to address
	; load address of $Aargc
	; op ? (0xf6) size=2
	; load address of $Aargv
	; op ? (0xef) size=2
	; load address of $Aargv
	ld hl, 0
	call add1616
	ld hl, 0
	call add1616
	; load word from address
	ld hl, 45
	call ueq1616
	; load address of $Aargv
	ld hl, 0
	call add1616
	ld hl, 2
	call add1616
	; load word from address
	ld hl, 73
	call ueq1616
	; load address of $Aargv
	ld hl, 0
	call add1616
	ld hl, 2
	call add1616
	; load word from address
	ld hl, 105
	call ueq1616
	; op h (0x68) size=2
	; load address of $Aargv
	ld hl, 0
	call add1616
	ld hl, 2
	call add1616
	; load word from address
	ld hl, 68
	call ueq1616
	; op h (0x68) size=2
	; op j (0x6a) size=2
	; load address of $cc1_argc
	; load word from address
	; op X (0x58) size=4
	ld hl, 2560
	call ge1616
	; load address of $_fprintf
	; load address of $_stderr
	; load address of $_str27
	; load word from address
	; op @ (0x40) size=2
	; load address of $_exit
	ld hl, 1
	; op @ (0x40) size=2
_if_29:
	; load address of $cc1_args
	; load address of $cc1_argc
	; op ? (0xef) size=2
	call add1616
	; load address of $Aargv
	ld hl, 0
	call add1616
	; load word from address
	; store byte to address
	; load address of $Aargc
	; op ? (0xf6) size=2
	; load address of $Aargv
	; op ? (0xef) size=2
	; load address of $_strcmp
	; load address of $Aargv
	ld hl, 0
	call add1616
	; load address of $_str28
	; load word from address
	; op @ (0x40) size=2
	ld hl, 0
	call eq1616
	; load address of $cc1_argc
	; load word from address
	; op X (0x58) size=4
	ld hl, 2560
	call ge1616
	; load address of $_fprintf
	; load address of $_stderr
	; load address of $_str29
	; load word from address
	; op @ (0x40) size=2
	; load address of $_exit
	ld hl, 1
	; op @ (0x40) size=2
_if_32:
	; load address of $cc1_args
	; load address of $cc1_argc
	; op ? (0xef) size=2
	call add1616
	; load address of $Aargv
	ld hl, 0
	call add1616
	; load word from address
	; store byte to address
	; load address of $Aargc
	; op ? (0xf6) size=2
	; load address of $Aargv
	; op ? (0xef) size=2
	; load address of $Aargv
	ld hl, 0
	call add1616
	ld hl, 0
	call add1616
	; load word from address
	ld hl, 45
	call ueq1616
	; load address of $_fprintf
	; load address of $_stderr
	; load address of $Aargv
	ld hl, 0
	call add1616
	; load word from address
	; load address of $_str30
	; load word from address
	; op @ (0x40) size=2
	; load address of $_usage
	; op @ (0x40) size=2
	; load address of $source_file
	; load word from address
	; load address of $_fprintf
	; load address of $_stderr
	; load address of $_str31
	; load word from address
	; op @ (0x40) size=2
	; load address of $_exit
	ld hl, 1
	; op @ (0x40) size=2
_if_36:
	; load address of $source_file
	; load address of $Aargv
	ld hl, 0
	call add1616
	; load word from address
	; store word to address
	; load address of $Aargc
	; op ? (0xf6) size=2
	; load address of $Aargv
	; op ? (0xef) size=2
_if_end_35:
_if_end_33:
_if_end_30:
_if_end_24:
_if_end_20:
_if_end_18:
_if_end_16:
_if_end_13:
_if_end_37:
	; load address of $source_file
	; load word from address
	; op ' (0x27) size=2 unsigned
	; load address of $_fprintf
	; load address of $_stderr
	; load address of $_str32
	; load word from address
	; op @ (0x40) size=2
	; load address of $_usage
	; op @ (0x40) size=2
_if_38:
	; load address of $_access
	; load address of $source_file
	ld hl, 4
	; load word from address
	; op @ (0x40) size=2
	ld hl, 0
	call ne1616
	; load address of $_fprintf
	; load address of $_stderr
	; load address of $source_file
	; load word from address
	; load address of $_str33
	; load word from address
	; op @ (0x40) size=2
	; load address of $_exit
	ld hl, 1
	; op @ (0x40) size=2
_if_39:
	; load address of $output_file
	; load word from address
	; op ' (0x27) size=2 unsigned
	; load address of $output_file
	; load address of $_str34
	; store word to address
_if_40:
	; load address of $basename_no_ext
	; load address of $_get_basename_no_ext
	; load address of $source_file
	; load word from address
	; op @ (0x40) size=2
	; store word to address
	; load address of $keep_ast
	; load word from address
	; load address of $ast_file
	; load address of $_malloc
	; load address of $_strlen
	; load address of $basename_no_ext
	; load word from address
	; op @ (0x40) size=2
	ld hl, 10
	call add1616
	; op @ (0x40) size=2
	; store word to address
	; load address of $_sprintf
	; load address of $ast_file
	; load address of $basename_no_ext
	; load word from address
	; load address of $_str35
	; load word from address
	; op @ (0x40) size=2
	; load address of $ast_file
	; load address of $_make_temp_ast
	; load address of $basename_no_ext
	; load word from address
	; op @ (0x40) size=2
	; store word to address
_if_end_42:
	; load address of $_printf
	; load address of $source_file
	; load word from address
	; load address of $_str36
	; op @ (0x40) size=2
	; load address of $cc1_args
	; load address of $cc1_argc
	; op ? (0xef) size=2
	call add1616
	; load address of $_str37
	; store byte to address
	; load address of $cc1_args
	; load address of $cc1_argc
	; op ? (0xef) size=2
	call add1616
	; load address of $ast_file
	; load word from address
	; store byte to address
	; load address of $cc1_args
	; load address of $cc1_argc
	; op ? (0xef) size=2
	call add1616
	; load address of $source_file
	; load word from address
	; store byte to address
	; load address of $cc1_args
	; load address of $cc1_argc
	; load word from address
	call add1616
	ld hl, 0
	; op N (0x4e) size=2 unsigned
	; store byte to address
	; load address of $status
	; load address of $_exec_command
	; load address of $cc1_args
	; load address of $cc1_path
	; op @ (0x40) size=2
	; store word to address
	; load address of $status
	; load word from address
	; op X (0x58) size=4
	ld hl, 0
	call ne1616
	; load address of $_fprintf
	; load address of $_stderr
	; load address of $status
	; load word from address
	; load address of $_str38
	; load word from address
	; op @ (0x40) size=2
	; load address of $keep_ast
	; load word from address
	; op ' (0x27) size=2
	; load address of $_unlink
	; load address of $ast_file
	; load word from address
	; op @ (0x40) size=2
_if_44:
	; load address of $_exit
	; load address of $status
	; load word from address
	; op @ (0x40) size=2
_if_43:
	; load address of $execute_ast
	; load word from address
	; load address of $interp_argc
	ld hl, 0
	; store word to address
	; load address of $_printf
	; load address of $_str39
	; op @ (0x40) size=2
	; load address of $interp_args
	; load address of $interp_argc
	; op ? (0xef) size=2
	call add1616
	; load address of $_str40
	; store byte to address
	; load address of $interp_args
	; load address of $interp_argc
	; op ? (0xef) size=2
	call add1616
	; load address of $_str41
	; store byte to address
	; load address of $interp_args
	; load address of $interp_argc
	; op ? (0xef) size=2
	call add1616
	; load address of $interp_path
	; store byte to address
	; load address of $interp_args
	; load address of $interp_argc
	; op ? (0xef) size=2
	call add1616
	; load address of $ast_file
	; load word from address
	; store byte to address
	; load address of $interp_args
	; load address of $interp_argc
	; load word from address
	call add1616
	ld hl, 0
	; op N (0x4e) size=2 unsigned
	; store byte to address
	; load address of $status
	; load address of $_exec_command
	; load address of $interp_args
	; load address of $_str42
	; op @ (0x40) size=2
	; store word to address
	; load address of $_printf
	; load address of $ast_file
	; load word from address
	; load address of $_str43
	; op @ (0x40) size=2
	; load address of $_free
	; load address of $ast_file
	; load word from address
	; op @ (0x40) size=2
	; load address of $_free
	; load address of $basename_no_ext
	; load word from address
	; op @ (0x40) size=2
	; load address of $_free
	; load address of $_scriptdir
	; load word from address
	; op @ (0x40) size=2
	; load address of $_exit
	; load address of $status
	; load word from address
	; op @ (0x40) size=2
_if_45:
	; load address of $_printf
	; load address of $ast_file
	; load word from address
	; load address of $_str44
	; op @ (0x40) size=2
	; load address of $cc2_args
	; load address of $cc2_argc
	; op ? (0xef) size=2
	call add1616
	; load address of $_str45
	; store byte to address
	; load address of $cc2_args
	; load address of $cc2_argc
	; op ? (0xef) size=2
	call add1616
	; load address of $output_file
	; load word from address
	; store byte to address
	; load address of $cc2_args
	; load address of $cc2_argc
	; op ? (0xef) size=2
	call add1616
	; load address of $ast_file
	; load word from address
	; store byte to address
	; load address of $cc2_args
	; load address of $cc2_argc
	; load word from address
	call add1616
	ld hl, 0
	; op N (0x4e) size=2 unsigned
	; store byte to address
	; load address of $status
	; load address of $_exec_command
	; load address of $cc2_args
	; load address of $cc2_path
	; op @ (0x40) size=2
	; store word to address
	; load address of $status
	; load word from address
	; op X (0x58) size=4
	ld hl, 0
	call ne1616
	; load address of $_fprintf
	; load address of $_stderr
	; load address of $status
	; load word from address
	; load address of $_str46
	; load word from address
	; op @ (0x40) size=2
	; load address of $keep_ast
	; load word from address
	; op ' (0x27) size=2
	; load address of $_unlink
	; load address of $ast_file
	; load word from address
	; op @ (0x40) size=2
_if_47:
	; load address of $_exit
	; load address of $status
	; load word from address
	; op @ (0x40) size=2
_if_46:
	; load address of $keep_ast
	; load word from address
	; op ' (0x27) size=2
	; load address of $_unlink
	; load address of $ast_file
	; load word from address
	; op @ (0x40) size=2
	; load address of $_printf
	; load address of $ast_file
	; load word from address
	; load address of $_str47
	; op @ (0x40) size=2
_if_end_49:
	; load address of $_printf
	; load address of $output_file
	; load word from address
	; load address of $_str48
	; op @ (0x40) size=2
	; load address of $_free
	; load address of $ast_file
	; load word from address
	; op @ (0x40) size=2
	; load address of $_free
	; load address of $basename_no_ext
	; load word from address
	; op @ (0x40) size=2
	; load address of $_free
	; load address of $_scriptdir
	; load word from address
	; op @ (0x40) size=2
	ld hl, 0
	call framefree
