; -------------------------------------------------------
;
; TTE - Bootblock
;
; -------------------------------------------------------


         incdir     include/
         include    "exec/memory.i"
         include    "exec/exec.i"

; sup bootblock

PUSHALL  MACRO
         movem.l    d0-a6,-(sp)
         ENDM

POPALL   MACRO
         movem.l    (sp)+,d0-a6
         ENDM

_LVOAllocMem      = -198

_ExecBase       EQU 4
_LVODoIo        EQU -456
;CMD_READ        EQU 2
;IO_COMMAND      EQU 28
;IO_DATA         EQU 40
;IO_LENGTH       EQU 36 
;IO_OFFSET       EQU 44


; Memory Alloc Flags
;MEMF_PUBLIC     EQU $0001
;MEMF_CHIP       EQU $0002
;MEMF_FAST       EQU $0004
;MEMF_CLEAR      EQU $10000

LOAD_ADDRESS      = $30000
UNPACK_ADDRESS    = $40000
DISK_MAX_FILES    = 4                                 ; bs value, but it'll do
FILE_TABLE_SIZE   = DISK_MAX_FILES*4*4

TEST              = 0
         IF         TEST=1
         bra        Main
         ENDIF

BootBlock:  
         dc.b       "DOS",0
         dc.l       0                                 ; checksum
         dc.b       "TTE!"                            ; spare?
			
Main:
         move.l     a1,-(sp)
         move.l     _ExecBase,a6
         bsr        FindMem
         move.l     d0,$84.w
         move.l     d1,$88.w

         bsr        disable_cache
         bsr        set_vbr

         move.l     (sp)+,a1
         move.l     _ExecBase,a6
         move.w     #CMD_READ,IO_COMMAND(a1)
         move.l     #LOAD_ADDRESS,IO_DATA(a1)
         move.l     #$400,IO_OFFSET(a1)
         move.l     #$200,IO_LENGTH(a1)
         jsr        _LVODoIo(a6)
         tst.l      d0
         bne        Error
            
         move.l     #"BOOT",d0
         bsr        LoadFile

         lea        LOAD_ADDRESS,a0
         lea        UNPACK_ADDRESS,a1
         lea        Progress(pc),a2
         moveq      #1,d7

         cmp.b      #1,d0
         beq        .shrink
         cmp.b      #2,d0
         beq        .zx0
         bra        Error

.zx0
         bsr        zx0_decompress
         bra        .jump

.shrink
         bsr        ShrinklerDecompress
.jump
         jmp        UNPACK_ADDRESS

Progress:
         dc.l       0

Error:   moveq      #-1,d7
.loop
         move.w     d7,$dff180
         dbra       d7,.loop
         bra        Error
			
;--------------------------------------
;
; Load File
;
; a0 = load address
; d0 = file name
;
; returns
; d0 = packing type
;
;--------------------------------------

LoadFile:
         lea        LOAD_ADDRESS,a0
         moveq      #DISK_MAX_FILES-1,d7
.loopfiles
         cmp.l      (a0),d0
         beq        .found
         lea        4*4(a0),a0
         dbra       d7,.loopfiles
         bra        Error

.found
         move.l     4(a0),d0                          ; disk position
         move.l     8(a0),d1                          ; load size
            
         move.l     d1,d2
         and.l      #$ffffff,d1                       ; and off cache / packing bits
         rol.l      #4,d2
         and.w      #$f,d2                            ; packing type bits
         move.l     d2,-(sp)

         move.l     #$1ff,d5


         move.l     d0,d2
         and.l      d5,d2
         eor.l      d2,d0
         add.l      d2,d1
         not.l      d5
         and.l      d5,d1
         add.l      #$200,d1
         move.l     #LOAD_ADDRESS,d3
         sub.l      d2,d3

         move.l     _ExecBase,a6
         move.w     #CMD_READ,IO_COMMAND(a1)
         move.l     d3,IO_DATA(a1)
         move.l     d0,IO_OFFSET(a1)
         move.l     d1,IO_LENGTH(a1)
         jsr        _LVODoIo(a6)
         tst.l      d0
         bne        Error

         move.w     #9,IO_COMMAND(a1)                 ; stop the drive
         clr.l      IO_LENGTH(a1)
         jsr        _LVODoIo(a6)

         move.l     (sp)+,d0                          ; packing type
         rts            


set_vbr  PUSHALL
         bsr        get_vbr
         tst.l      d0
         beq        .ok

         move.l     #4,a1
         move.l     d0,a0
         move.w     #$c0-5,d7
.loop
         move.b     (a0)+,(a1)+
         dbra       d7,.loop

         moveq      #0,d0
         dc.l       $4e7b0801                         ; movec d0,vbr
.ok
         POPALL
         rts


get_vbr  move.l     a5,-(a7)
         moveq      #0,d0                             ; default at $0
         move.l     $4.w,a6
         btst       #0,296+1(a6)                      ; 68010+?
         beq.b      .is68k                            ; nope.
         lea        .getit(pc),a5
         jsr        -30(a6)                           ; SuperVisor()
.is68k   move.l     (a7)+,a5
         rts

.getit   dc.l       $4E7A0801                         ;      vbr,d0
         rte                                          ; back to user state code
	

disable_cache
         PUSHALL
         move.w     #$4000,$dff09a
         movea.l    $4.w,a6
         lea        super(pc),a5
         jsr        -$1e(a6)
         move.w     #$c000,$dff09a
         POPALL
         rts

super
         moveq      #0,d0
         lea        trap(pc),a5
         lea        $10.w,a1
         lea        $2c.w,a2
         movea.l    (a1),a3
         movea.l    (a2),a4
         move.l     a5,(a1)
         move.l     a5,(a2)
            ;dc.l       $4e7b0801                                                    ;movec d0,vbr
         dc.l       $f4784e71                         ;cpusha dc
         dc.l       $4e7b0002                         ;movec d0,cacr
         dc.l       $4e7b0808                         ;movec d0,pcr
         move.l     a3,(a1)
         move.l     a4,(a2)
         rte
trap     addq.l     #4,2(sp)
         rte



;--------------------------------------
;
; FindMem
;
; finds the largest contiguous block of memory above 512k of chip
;
; d0 = start of ram
; d1 = end of ram
;
; both returned zero if no additional ram could be found
;
;--------------------------------------

FindMem:   
         move.l     MemList(a6),a1
         moveq      #0,d0
         moveq      #0,d1
         moveq      #0,d4

.loop:    
         move.l     a1,a0
         move.l     MH_LOWER(a1),d2
         move.l     MH_UPPER(a1),d3

         and.l      #$fff80000,d2
         add.l      #$10000,d3
         and.l      #$fff80000,d3

		
         btst.b     #MEMB_CHIP,MH_ATTRIBUTES+1(a1)    ; Is this the chip mem? 
         beq.s      .notchip

         add.l      #$80000,d2                        ; yes, ignore first 512k of chip ram

.notchip: 
         move.l     d3,d5
         sub.l      d2,d5                             ; this block size
         cmp.l      d4,d5                             ; is it bigger?
         bcs        .smaller

         move.l     d2,d0                             ; yes, use that
         move.l     d3,d1
         move.l     d5,d4

.smaller
         move.l     LN_SUCC(a1),a1                    ; next block..
         tst.l      (a1)
         bne.s      .loop

.exit
         rts

         include    "ShrinklerDecompress.asm"
         include    "unzx0_68000.asm"
		
BootBlockEnd:
SPACER            = $400-BootBlockEnd-BootBlock	
         dcb.b      SPACER	