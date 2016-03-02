package com.clarify.pricer.dto;

import java.sql.Date;

public class Cbsa {
	public static final String SIZE_LARGE_URBAN = "L";
	public static final String SIZE_OTHER_URBAN = "O";
	public static final String SIZE_ALL_RURAL = "R";

	private String f_cbsa;
	private String f_cbsa_size;
	private Date f_cbsa_eff_date;
	private double f_cbsa_wage_indx1;
	private double f_cbsa_wage_indx2;
	private String f_cbsa_state_name;

	public String getF_cbsa() {
		return f_cbsa;
	}

	public void setF_cbsa(String f_cbsa) {
		this.f_cbsa = f_cbsa;
	}

	public String getF_cbsa_size() {
		return f_cbsa_size;
	}

	public void setF_cbsa_size(String f_cbsa_size) {
		this.f_cbsa_size = f_cbsa_size;
	}

	public Date getF_cbsa_eff_date() {
		return f_cbsa_eff_date;
	}

	public void setF_cbsa_eff_date(Date f_cbsa_eff_date) {
		this.f_cbsa_eff_date = f_cbsa_eff_date;
	}

	public double getF_cbsa_wage_indx1() {
		return f_cbsa_wage_indx1;
	}

	public void setF_cbsa_wage_indx1(double f_cbsa_wage_indx1) {
		this.f_cbsa_wage_indx1 = f_cbsa_wage_indx1;
	}

	public double getF_cbsa_wage_indx2() {
		return f_cbsa_wage_indx2;
	}

	public void setF_cbsa_wage_indx2(double f_cbsa_wage_indx2) {
		this.f_cbsa_wage_indx2 = f_cbsa_wage_indx2;
	}

	public String getF_cbsa_state_name() {
		return f_cbsa_state_name;
	}

	public void setF_cbsa_state_name(String f_cbsa_state_name) {
		this.f_cbsa_state_name = f_cbsa_state_name;
	}
}
