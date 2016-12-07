; RUN: llc -march=hexagon -disable-hsdr -hexagon-expand-condsets=0 -hexagon-bit=0 -disable-post-ra < %s | FileCheck %s

; CHECK-DAG: r[[HI0:[0-9]+]]:[[LO0:[0-9]+]] = combine(#0, #0)
; CHECK-DAG: r[[HI1:[0-9]+]]:[[LO1:[0-9]+]] = combine(#0, #1)
; CHECK: r[[LOSUM:[0-9:]+]] = add(r5:4, r1:0)
; CHECK: p[[CARRYFLAG:[0-9]+]] = cmp.gtu(r1:0, r5:4)
; CHECK: r[[LOSUM:[0-9:]+]] = add(r{{[0-9:]+}}, r3:2)
; CHECK: r[[LOCARRY:[0-9]+]] = mux(p[[CARRYFLAG]], r[[LO1]], r[[LO0]])
; CHECK: r[[HICARRY:[0-9]+]] = mux(p[[CARRYFLAG]], r[[HI1]], r[[HI0]])
; CHECK: r[[CARRY:[0-9:]+]] = combine(r[[HICARRY]], r[[LOCARRY]])
; CHECK: r[[HISUM:[0-9:]+]] = add(r3:2, r[[CARRY]])

define void @check_adde_addc (i64 %AL, i64 %AH, i64 %BL, i64 %BH, i64* %RL, i64* %RH) {
entry:
        %tmp1 = zext i64 %AL to i128
        %tmp23 = zext i64 %AH to i128
        %tmp4 = shl i128 %tmp23, 64
        %tmp5 = or i128 %tmp4, %tmp1
        %tmp67 = zext i64 %BL to i128
        %tmp89 = zext i64 %BH to i128
        %tmp11 = shl i128 %tmp89, 64
        %tmp12 = or i128 %tmp11, %tmp67
        %tmp15 = add i128 %tmp12, %tmp5
        %tmp1617 = trunc i128 %tmp15 to i64
        store i64 %tmp1617, i64* %RL
        %tmp21 = lshr i128 %tmp15, 64
        %tmp2122 = trunc i128 %tmp21 to i64
        store i64 %tmp2122, i64* %RH
        ret void
}
