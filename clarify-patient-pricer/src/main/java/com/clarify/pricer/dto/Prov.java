package com.clarify.pricer.dto;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

public class Prov {
	private String prov_plus1;
	private String p_npi8;
	private String p_npi_filler;
	private String p_provider_no;
	private Date p_effect_date;
	private Date p_fy_beginning_date;
	private Date p_report_date;
	private Date p_termination_date;
	private String p_waiver_code;
	private Integer p_inter_no;
	private String p_provider_type;
	private Integer p_current_census_div;
	private String p_chg_code_index;
	private String p_msa_x;
	private String p_wage_index_loc_msa;
	private String p_stand_amt_loc_msa;
	private String p_sol_com_dep_hosp_yr;
	private String p_lugar;
	private String p_temp_relief_ind;
	private String p_fed_pps_blend;
	private Double p_cmi_adj_cpd;
	private BigDecimal p_cola;
	private BigDecimal p_intern_ratio;
	private Integer p_bed_size;
	private Double p_ccr;
	private Double p_cmi;
	private Double p_ssi_ratio;
	private Double p_medicaid_ratio;
	private String p_pps_blend_yr_ind;
	private Double p_prup_update_factor;
	private Double p_dsh_percent;
	private Date p_fye_date;
	private String p_cbsa_spec_pay_ind;
	private String p_cbsa_hosp_qual_ind;
	private String p_cbsa_geo_loc;
	private String p_cbsa_reclass_loc;
	private String p_cbsa_stand_amt_loc;
	private BigDecimal p_cbsa_spec_wi;
	private Double p_pass_amt_capital;
	private Double p_pass_amt_dir_med_ed;
	private Double p_pass_amt_organ_acq;
	private Double p_pass_amt_plus_misc;
	private String p_capi_pps_pay_code;
	private BigDecimal p_capi_hosp_spec_rate;
	private Double p_capi_old_harm_rate;
	private Double p_capi_new_harm_ratio;
	private Double p_capi_cstchg_ratio;
	private String p_capi_new_hosp;
	private Double p_capi_ime;
	private Double p_capi_exceptions;
	private String p_val_based_purch_partipnt;
	private Double p_val_based_purch_adjust;
	private String p_hosp_readmission_redu;
	private String p_hosp_hrr_adjustmt;
	private Double p_model1_bundle_disprcnt;
	private String p_hac_reduc_ind;
	private Double p_uncomp_care_amount;
	private String p_ehr_reduc_ind;
	private String p_prov_name;

	public String getProv_plus1() {
		return prov_plus1;
	}

	public void setProv_plus1(String prov_plus1) {
		this.prov_plus1 = prov_plus1;
	}

	public String getP_npi8() {
		return p_npi8;
	}

	public void setP_npi8(String p_npi8) {
		this.p_npi8 = p_npi8;
	}

	public String getP_npi_filler() {
		return p_npi_filler;
	}

	public void setP_npi_filler(String p_npi_filler) {
		this.p_npi_filler = p_npi_filler;
	}

	public String getP_provider_no() {
		return p_provider_no;
	}

	public void setP_provider_no(String p_provider_no) {
		this.p_provider_no = p_provider_no;
	}

	public Date getP_effect_date() {
		return p_effect_date;
	}

	public void setP_effect_date(Date p_effect_date) {
		this.p_effect_date = p_effect_date;
	}

	public Date getP_fy_beginning_date() {
		return p_fy_beginning_date;
	}

	public void setP_fy_beginning_date(Date p_fy_beginning_date) {
		this.p_fy_beginning_date = p_fy_beginning_date;
	}

	public Date getP_report_date() {
		return p_report_date;
	}

	public void setP_report_date(Date p_report_date) {
		this.p_report_date = p_report_date;
	}

	public Date getP_termination_date() {
		return p_termination_date;
	}

	public void setP_termination_date(Date p_termination_date) {
		this.p_termination_date = p_termination_date;
	}

	public String getP_waiver_code() {
		return p_waiver_code;
	}

	public void setP_waiver_code(String p_waiver_code) {
		this.p_waiver_code = p_waiver_code;
	}

	public Integer getP_inter_no() {
		return p_inter_no;
	}

	public void setP_inter_no(Integer p_inter_no) {
		this.p_inter_no = p_inter_no;
	}

	public String getP_provider_type() {
		return p_provider_type;
	}

	public void setP_provider_type(String p_provider_type) {
		this.p_provider_type = p_provider_type;
	}

	public Integer getP_current_census_div() {
		return p_current_census_div;
	}

	public void setP_current_census_div(Integer p_current_census_div) {
		this.p_current_census_div = p_current_census_div;
	}

	public String getP_chg_code_index() {
		return p_chg_code_index;
	}

	public void setP_chg_code_index(String p_chg_code_index) {
		this.p_chg_code_index = p_chg_code_index;
	}

	public String getP_msa_x() {
		return p_msa_x;
	}

	public void setP_msa_x(String p_msa_x) {
		this.p_msa_x = p_msa_x;
	}

	public String getP_wage_index_loc_msa() {
		return p_wage_index_loc_msa;
	}

	public void setP_wage_index_loc_msa(String p_wage_index_loc_msa) {
		this.p_wage_index_loc_msa = p_wage_index_loc_msa;
	}

	public String getP_stand_amt_loc_msa() {
		return p_stand_amt_loc_msa;
	}

	public void setP_stand_amt_loc_msa(String p_stand_amt_loc_msa) {
		this.p_stand_amt_loc_msa = p_stand_amt_loc_msa;
	}

	public String getP_sol_com_dep_hosp_yr() {
		return p_sol_com_dep_hosp_yr;
	}

	public void setP_sol_com_dep_hosp_yr(String p_sol_com_dep_hosp_yr) {
		this.p_sol_com_dep_hosp_yr = p_sol_com_dep_hosp_yr;
	}

	public String getP_lugar() {
		return p_lugar;
	}

	public void setP_lugar(String p_lugar) {
		this.p_lugar = p_lugar;
	}

	public String getP_temp_relief_ind() {
		return p_temp_relief_ind;
	}

	public void setP_temp_relief_ind(String p_temp_relief_ind) {
		this.p_temp_relief_ind = p_temp_relief_ind;
	}

	public String getP_fed_pps_blend() {
		return p_fed_pps_blend;
	}

	public void setP_fed_pps_blend(String p_fed_pps_blend) {
		this.p_fed_pps_blend = p_fed_pps_blend;
	}

	public Double getP_cmi_adj_cpd() {
		return p_cmi_adj_cpd;
	}

	public void setP_cmi_adj_cpd(Double p_cmi_adj_cpd) {
		this.p_cmi_adj_cpd = p_cmi_adj_cpd;
	}

	public BigDecimal getP_cola() {
		return p_cola;
	}

	public void setP_cola(BigDecimal p_cola) {
		this.p_cola = p_cola;
	}

	public BigDecimal getP_intern_ratio() {
		return p_intern_ratio;
	}

	public void setP_intern_ratio(BigDecimal p_intern_ratio) {
		this.p_intern_ratio = p_intern_ratio;
	}

	public Integer getP_bed_size() {
		return p_bed_size;
	}

	public void setP_bed_size(Integer p_bed_size) {
		this.p_bed_size = p_bed_size;
	}

	public Double getP_ccr() {
		return p_ccr;
	}

	public void setP_ccr(Double p_ccr) {
		this.p_ccr = p_ccr;
	}

	public Double getP_cmi() {
		return p_cmi;
	}

	public void setP_cmi(Double p_cmi) {
		this.p_cmi = p_cmi;
	}

	public Double getP_ssi_ratio() {
		return p_ssi_ratio;
	}

	public void setP_ssi_ratio(Double p_ssi_ratio) {
		this.p_ssi_ratio = p_ssi_ratio;
	}

	public Double getP_medicaid_ratio() {
		return p_medicaid_ratio;
	}

	public void setP_medicaid_ratio(Double p_medicaid_ratio) {
		this.p_medicaid_ratio = p_medicaid_ratio;
	}

	public String getP_pps_blend_yr_ind() {
		return p_pps_blend_yr_ind;
	}

	public void setP_pps_blend_yr_ind(String p_pps_blend_yr_ind) {
		this.p_pps_blend_yr_ind = p_pps_blend_yr_ind;
	}

	public Double getP_prup_update_factor() {
		return p_prup_update_factor;
	}

	public void setP_prup_update_factor(Double p_prup_update_factor) {
		this.p_prup_update_factor = p_prup_update_factor;
	}

	public Double getP_dsh_percent() {
		return p_dsh_percent;
	}

	public void setP_dsh_percent(Double p_dsh_percent) {
		this.p_dsh_percent = p_dsh_percent;
	}

	public Date getP_fye_date() {
		return p_fye_date;
	}

	public void setP_fye_date(Date p_fye_date) {
		this.p_fye_date = p_fye_date;
	}

	public String getP_cbsa_spec_pay_ind() {
		return p_cbsa_spec_pay_ind;
	}

	public void setP_cbsa_spec_pay_ind(String p_cbsa_spec_pay_ind) {
		this.p_cbsa_spec_pay_ind = p_cbsa_spec_pay_ind;
	}

	public String getP_cbsa_hosp_qual_ind() {
		return p_cbsa_hosp_qual_ind;
	}

	public void setP_cbsa_hosp_qual_ind(String p_cbsa_hosp_qual_ind) {
		this.p_cbsa_hosp_qual_ind = p_cbsa_hosp_qual_ind;
	}

	public String getP_cbsa_geo_loc() {
		return p_cbsa_geo_loc;
	}

	public void setP_cbsa_geo_loc(String p_cbsa_geo_loc) {
		this.p_cbsa_geo_loc = p_cbsa_geo_loc;
	}

	public String getP_cbsa_reclass_loc() {
		// INDRV161 > 1350-CHECK-CBSA
		if (p_cbsa_reclass_loc == null || "".equals(p_cbsa_reclass_loc.trim())
				|| "00000".equals("00000")) {
			return p_cbsa_geo_loc;
		}
		return p_cbsa_reclass_loc;
	}

	public void setP_cbsa_reclass_loc(String p_cbsa_reclass_loc) {
		this.p_cbsa_reclass_loc = p_cbsa_reclass_loc;
	}

	public String getP_cbsa_stand_amt_loc() {
		// INDRV161 > 1350-CHECK-CBSA
		if (p_cbsa_stand_amt_loc == null
				|| "".equals(p_cbsa_stand_amt_loc.trim())
				|| "00000".equals(p_cbsa_stand_amt_loc)) {
			return p_cbsa_geo_loc;
		}
		return p_cbsa_stand_amt_loc;
	}

	public void setP_cbsa_stand_amt_loc(String p_cbsa_stand_amt_loc) {
		this.p_cbsa_stand_amt_loc = p_cbsa_stand_amt_loc;
	}

	public BigDecimal getP_cbsa_spec_wi() {
		return p_cbsa_spec_wi;
	}

	public void setP_cbsa_spec_wi(BigDecimal p_cbsa_spec_wi) {
		this.p_cbsa_spec_wi = p_cbsa_spec_wi;
	}

	public Double getP_pass_amt_capital() {
		return p_pass_amt_capital;
	}

	public void setP_pass_amt_capital(Double p_pass_amt_capital) {
		this.p_pass_amt_capital = p_pass_amt_capital;
	}

	public Double getP_pass_amt_dir_med_ed() {
		return p_pass_amt_dir_med_ed;
	}

	public void setP_pass_amt_dir_med_ed(Double p_pass_amt_dir_med_ed) {
		this.p_pass_amt_dir_med_ed = p_pass_amt_dir_med_ed;
	}

	public Double getP_pass_amt_organ_acq() {
		return p_pass_amt_organ_acq;
	}

	public void setP_pass_amt_organ_acq(Double p_pass_amt_organ_acq) {
		this.p_pass_amt_organ_acq = p_pass_amt_organ_acq;
	}

	public Double getP_pass_amt_plus_misc() {
		return p_pass_amt_plus_misc;
	}

	public void setP_pass_amt_plus_misc(Double p_pass_amt_plus_misc) {
		this.p_pass_amt_plus_misc = p_pass_amt_plus_misc;
	}

	public String getP_capi_pps_pay_code() {
		return p_capi_pps_pay_code;
	}

	public void setP_capi_pps_pay_code(String p_capi_pps_pay_code) {
		this.p_capi_pps_pay_code = p_capi_pps_pay_code;
	}

	public BigDecimal getP_capi_hosp_spec_rate() {
		return p_capi_hosp_spec_rate;
	}

	public void setP_capi_hosp_spec_rate(BigDecimal p_capi_hosp_spec_rate) {
		this.p_capi_hosp_spec_rate = p_capi_hosp_spec_rate;
	}

	public Double getP_capi_old_harm_rate() {
		return p_capi_old_harm_rate;
	}

	public void setP_capi_old_harm_rate(Double p_capi_old_harm_rate) {
		this.p_capi_old_harm_rate = p_capi_old_harm_rate;
	}

	public Double getP_capi_new_harm_ratio() {
		return p_capi_new_harm_ratio;
	}

	public void setP_capi_new_harm_ratio(Double p_capi_new_harm_ratio) {
		this.p_capi_new_harm_ratio = p_capi_new_harm_ratio;
	}

	public Double getP_capi_cstchg_ratio() {
		return p_capi_cstchg_ratio;
	}

	public void setP_capi_cstchg_ratio(Double p_capi_cstchg_ratio) {
		this.p_capi_cstchg_ratio = p_capi_cstchg_ratio;
	}

	public String getP_capi_new_hosp() {
		return p_capi_new_hosp;
	}

	public void setP_capi_new_hosp(String p_capi_new_hosp) {
		this.p_capi_new_hosp = p_capi_new_hosp;
	}

	public Double getP_capi_ime() {
		return p_capi_ime;
	}

	public void setP_capi_ime(Double p_capi_ime) {
		this.p_capi_ime = p_capi_ime;
	}

	public Double getP_capi_exceptions() {
		return p_capi_exceptions;
	}

	public void setP_capi_exceptions(Double p_capi_exceptions) {
		this.p_capi_exceptions = p_capi_exceptions;
	}

	public String getP_val_based_purch_partipnt() {
		return p_val_based_purch_partipnt;
	}

	public void setP_val_based_purch_partipnt(String p_val_based_purch_partipnt) {
		this.p_val_based_purch_partipnt = p_val_based_purch_partipnt;
	}

	public Double getP_val_based_purch_adjust() {
		return p_val_based_purch_adjust;
	}

	public void setP_val_based_purch_adjust(Double p_val_based_purch_adjust) {
		this.p_val_based_purch_adjust = p_val_based_purch_adjust;
	}

	public String getP_hosp_readmission_redu() {
		return p_hosp_readmission_redu;
	}

	public void setP_hosp_readmission_redu(String p_hosp_readmission_redu) {
		this.p_hosp_readmission_redu = p_hosp_readmission_redu;
	}

	public String getP_hosp_hrr_adjustmt() {
		return p_hosp_hrr_adjustmt;
	}

	public void setP_hosp_hrr_adjustmt(String p_hosp_hrr_adjustmt) {
		this.p_hosp_hrr_adjustmt = p_hosp_hrr_adjustmt;
	}

	public Double getP_model1_bundle_disprcnt() {
		return p_model1_bundle_disprcnt;
	}

	public void setP_model1_bundle_disprcnt(Double p_model1_bundle_disprcnt) {
		this.p_model1_bundle_disprcnt = p_model1_bundle_disprcnt;
	}

	public String getP_hac_reduc_ind() {
		return p_hac_reduc_ind;
	}

	public void setP_hac_reduc_ind(String p_hac_reduc_ind) {
		this.p_hac_reduc_ind = p_hac_reduc_ind;
	}

	public Double getP_uncomp_care_amount() {
		return p_uncomp_care_amount;
	}

	public void setP_uncomp_care_amount(Double p_uncomp_care_amount) {
		this.p_uncomp_care_amount = p_uncomp_care_amount;
	}

	public String getP_ehr_reduc_ind() {
		return p_ehr_reduc_ind;
	}

	public void setP_ehr_reduc_ind(String p_ehr_reduc_ind) {
		this.p_ehr_reduc_ind = p_ehr_reduc_ind;
	}

	public String getP_prov_name() {
		return p_prov_name;
	}

	public void setP_prov_name(String p_prov_name) {
		this.p_prov_name = p_prov_name;
	}

	// *****

	public boolean isIndianHealtService() {
		return "08".equals(p_provider_type);
	}

	public boolean isCbsaStdRuralCheck() {
		return p_cbsa_stand_amt_loc != null
				&& p_cbsa_stand_amt_loc.startsWith("   ");
	}

	public String getState() {
		return p_provider_no.substring(0, 2);
	}

	public boolean isAlaska() {
		return "02".equals(getState());
	}

	public boolean isHawaii() {
		return "12".equals(getState());
	}

	public boolean isPuertoRico() {
		return "40".equals(getState());
	}

	/**
	 * <code>88 P-N-MDH-REBASED-FY90 VALUE '14' '15'.</code>
	 * 
	 * @return
	 */
	public boolean isMdhRebasedFY90() {
		return "14".equals(p_provider_type) || "15".equals(p_provider_type);
	}

	/**
	 * <code>88 P-N-SCH-REBASED-FY90 VALUE '16' '17'.</code>
	 * 
	 * @return
	 */
	public boolean isSchRebasedFY90() {
		return "16".equals(p_provider_type) || "17".equals(p_provider_type);
	}

	/**
	 * <code>88 P-N-EACH VALUE '21' '22'.</code>
	 * 
	 * @return
	 */
	public boolean isEach() {
		return "21".equals(p_provider_type) || "22".equals(p_provider_type);
	}

	/**
	 * <p>
	 * PROV.B-FORMER-MDH-PROVIDERS as 161
	 * </p>
	 */
	protected static List<String> FORMER_MDH_ROVIDERS = new ArrayList<>(
			Arrays.asList(new String[] { "080006", "140184", "390072",
					"420019", "440031", "450451", "490019", "510062" }));

	public boolean isFormerMdhProvider() {
		return FORMER_MDH_ROVIDERS.indexOf(p_provider_no) != -1;
	}
}
