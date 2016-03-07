package com.clarify.pricer;

import java.math.BigDecimal;

/**
 * <p>
 * Java version of PPS-DATA found in PPCAL as 154/161.
 * </p>
 * 
 * @author ssamayoa
 *
 */
public class PPSData {
	// 01 PPS-DATA.
	// 10 PPS-RTC PIC 9(02).
	// 10 PPS-WAGE-INDX PIC 9(02)V9(04).
	private BigDecimal wageIndx;
	// 10 PPS-OUTLIER-DAYS PIC 9(03).
	private int outlinerDays;
	// 10 PPS-AVG-LOS PIC 9(02)V9(01).
	private BigDecimal avgLos;
	// 10 PPS-DAYS-CUTOFF PIC 9(02)V9(01).
	private BigDecimal daysCutoff;
	// 10 PPS-OPER-IME-ADJ PIC 9(06)V9(02).
	private BigDecimal operImeAdj;
	// 10 PPS-TOTAL-PAYMENT PIC 9(07)V9(02).
	private BigDecimal totalPayment;
	// 10 PPS-OPER-HSP-PART PIC 9(06)V9(02).
	private BigDecimal operHspPart;
	// 10 PPS-OPER-FSP-PART PIC 9(06)V9(02).
	private BigDecimal operFspPart;
	// 10 PPS-OPER-OUTLIER-PART PIC 9(07)V9(02).
	private BigDecimal operOutlinerPart;
	// 10 PPS-REG-DAYS-USED PIC 9(03).
	private int regDaysUsed;
	// 10 PPS-LTR-DAYS-USED PIC 9(02).
	private int ltrDaysUsed;
	// 10 PPS-OPER-DSH-ADJ PIC 9(06)V9(02).
	private BigDecimal operDshAdj;

	// 10 PPS-CALC-VERS PIC X(05).
	public BigDecimal getWageIndx() {
		return wageIndx;
	}

	public void setWageIndx(BigDecimal wageIndx) {
		this.wageIndx = wageIndx;
	}

	public int getOutlinerDays() {
		return outlinerDays;
	}

	public void setOutlinerDays(int outlinerDays) {
		this.outlinerDays = outlinerDays;
	}

	public BigDecimal getAvgLos() {
		return avgLos;
	}

	public void setAvgLos(BigDecimal avgLos) {
		this.avgLos = avgLos;
	}

	public BigDecimal getDaysCutoff() {
		return daysCutoff;
	}

	public void setDaysCutoff(BigDecimal daysCutoff) {
		this.daysCutoff = daysCutoff;
	}

	public BigDecimal getOperImeAdj() {
		return operImeAdj;
	}

	public void setOperImeAdj(BigDecimal operImeAdj) {
		this.operImeAdj = operImeAdj;
	}

	public BigDecimal getTotalPayment() {
		return totalPayment;
	}

	public void setTotalPayment(BigDecimal totalPayment) {
		this.totalPayment = totalPayment;
	}

	public BigDecimal getOperHspPart() {
		return operHspPart;
	}

	public void setOperHspPart(BigDecimal operHspPart) {
		this.operHspPart = operHspPart;
	}

	public BigDecimal getOperFspPart() {
		return operFspPart;
	}

	public void setOperFspPart(BigDecimal operFspPart) {
		this.operFspPart = operFspPart;
	}

	public BigDecimal getOperOutlinerPart() {
		return operOutlinerPart;
	}

	public void setOperOutlinerPart(BigDecimal operOutlinerPart) {
		this.operOutlinerPart = operOutlinerPart;
	}

	public int getRegDaysUsed() {
		return regDaysUsed;
	}

	public void setRegDaysUsed(int regDaysUsed) {
		this.regDaysUsed = regDaysUsed;
	}

	public int getLtrDaysUsed() {
		return ltrDaysUsed;
	}

	public void setLtrDaysUsed(int ltrDaysUsed) {
		this.ltrDaysUsed = ltrDaysUsed;
	}

	public BigDecimal getOperDshAdj() {
		return operDshAdj;
	}

	public void setOperDshAdj(BigDecimal operDshAdj) {
		this.operDshAdj = operDshAdj;
	}
}
