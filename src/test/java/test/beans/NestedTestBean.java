package test.beans;

public class NestedTestBean implements INestedTestBean {

	private String company = "";

	public NestedTestBean() {
	}

	public NestedTestBean(String company) {
		setCompany(company);
	}

	@Override
	public String getCompany() {
		return company;
	}

	public void setCompany(String company) {
		this.company = (company != null ? company : "");
	}

	@Override
	public int hashCode() {
		return company.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof NestedTestBean)) {
			return false;
		}
		NestedTestBean ntb = (NestedTestBean) obj;
		return company.equals(ntb.company);
	}

	@Override
	public String toString() {
		return "NestedTestBean: " + company;
	}
	
}
