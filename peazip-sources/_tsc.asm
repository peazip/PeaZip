; *************************************************************************
;
; DESCRIPTION     :  RDTSC helper routines for 16 bit
;
; REQUIREMENTS    :  ---
;
; EXTERNAL DATA   :  ---
;
; MEMORY USAGE    :  ---
;
; REFERENCES      :  ---
;
; ASSEMBLER       :  ml /c
;
; Version  Date      Author      Modification
; -------  --------  -------     ------------------------------------------
; 0.10     16.11.03  W.Ehrhardt  initial version
; 0.11     16.11.03  we          use ml = MASM 6.14
; 0.12     02.01.04  we          Second cpuid after RDTSC
; **************************************************************************)


.586p

code segment public use16

	public 	_CheckCPUID,_CheckRDTSC,_RDTSC

;---------------------------------------------------------------------------
_CheckCPUID proc far
;check if CPUID supported
  	pushfd
  	pushfd
  	pop 	eax
  	mov     ecx,eax
  	xor     eax,200000h
 	push    eax
  	popfd
  	pushfd
  	pop    	eax
  	popfd
  	xor   	eax,ecx
  	setnz   al
  	ret
_CheckCPUID endp


;---------------------------------------------------------------------------
_CheckRDTSC proc far
;check if RDTSC supported
  	mov    eax,1
  	cpuid
  	test   dx,10h           ;test RDTSC flag in Features
  	setnz  al
  	ret
_CheckRDTSC endp


;---------------------------------------------------------------------------
_RDTSC	proc far pascal pCnt:dword
;Read Time Stamp Counter
    	xor   	eax, eax
    	xor   	ebx, ebx
    	xor   	ecx, ecx
    	xor   	edx, edx
	cpuid
        rdtsc
	les   	di,[pcnt]
	mov	es:[di],eax
	mov	es:[di+4],edx
    	xor   	eax, eax
	cpuid
	ret
_RDTSC	endp

code ends

	end

