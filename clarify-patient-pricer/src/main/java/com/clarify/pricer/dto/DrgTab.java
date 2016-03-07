package com.clarify.pricer.dto;

import java.math.BigDecimal;

public class DrgTab {
	private String drg_code;
	private BigDecimal drg_weight;
	private BigDecimal drg_gmalos;
	private Double drg_low;
	private BigDecimal drg_arith_alos;
	private String drg_pac;
	private String drg_sppac;
	private String drg_desc;

	public String getDrg_code() {
		return drg_code;
	}

	public void setDrg_code(String drg_code) {
		this.drg_code = drg_code;
	}

	public BigDecimal getDrg_weight() {
		return drg_weight;
	}

	public void setDrg_weight(BigDecimal drg_weight) {
		this.drg_weight = drg_weight;
	}

	public BigDecimal getDrg_gmalos() {
		return drg_gmalos;
	}

	public void setDrg_gmalos(BigDecimal drg_gmalos) {
		this.drg_gmalos = drg_gmalos;
	}

	public Double getDrg_low() {
		return drg_low;
	}

	public void setDrg_low(Double drg_low) {
		this.drg_low = drg_low;
	}

	public BigDecimal getDrg_arith_alos() {
		return drg_arith_alos;
	}

	public void setDrg_arith_alos(BigDecimal drg_arith_alos) {
		this.drg_arith_alos = drg_arith_alos;
	}

	public String getDrg_pac() {
		return drg_pac;
	}

	public void setDrg_pac(String drg_pac) {
		this.drg_pac = drg_pac;
	}

	public boolean isDrg_pac() {
		return "Y".equalsIgnoreCase(drg_pac);
	}

	public String getDrg_sppac() {
		return drg_sppac;
	}

	public void setDrg_sppac(String drg_sppac) {
		this.drg_sppac = drg_sppac;
	}

	public boolean isDrg_sppac() {
		return "Y".equalsIgnoreCase(drg_sppac);
	}

	public String getDrg_desc() {
		return drg_desc;
	}

	public void setDrg_desc(String drg_desc) {
		this.drg_desc = drg_desc;
	}

	// 88 D-DRG-POSTACUTE-PERDIEM VALUE 'Y '.
	public boolean isDrgPostacutePerdiem() {
		return "Y".equals(drg_pac) && " ".equals(drg_sppac);
	}

	// 88 D-DRG-POSTACUTE-50-50 VALUE 'Y Y'.
	public boolean isDrgPostacute5050() {
		return "Y".equals(drg_pac) && "Y".equals(drg_sppac);
	}

}
