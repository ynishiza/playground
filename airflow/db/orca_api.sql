BEGIN;

CREATE TYPE orca_sex AS ENUM ('M', 'F');

CREATE TABLE orca_income(
  _id UUID PRIMARY KEY DEFAULT uuidv7(),
  patient_id VARCHAR(20) NOT NULL,
  -- WholeName 患者氏名 (PII)
  -- WholeName_inKana 患者カナ氏名 (PII)
  birthdate DATE NOT NULL,
  sex orca_sex NOT NULL,
  income_information_overflow BOOLEAN,
  -- These columns are omitted since this will always provide
  -- the latest unpaid amount at the time of the API call,
  -- not the time that the invoice was issued.
  --
  -- Unpaid_Money_Total 未収金額合計
  -- Unpaid_Money_Information_Overflow 未収金情報オーバーフラグ
  perform_date DATE NOT NULL,
  perform_end_date DATE,
  issueddate DATE NOT NULL,
  inout CHAR(1) NOT NULL,
  invoice_number INTEGER NOT NULL,
  group_invoice_number INTEGER,
  insurance_combination_number SMALLINT NOT NULL,
  rate_cd NUMERIC(5,2) NOT NULL,
  department_code CHAR(2) NOT NULL,
  department_name VARCHAR(100) NOT NULL,
  ac_money NUMERIC(12,2) NOT NULL,
  tax_in_ac_money NUMERIC(12,2),
  ic_money NUMERIC(12,2) NOT NULL,
  ai_money NUMERIC(12,2) NOT NULL,
  oe_money NUMERIC(12,2) NOT NULL,
  dg_smoney NUMERIC(12,2),
  om_smoney NUMERIC(12,2),
  pi_smoney NUMERIC(12,2),
  ml_smoney NUMERIC(12,2),
  meal_smoney NUMERIC(12,2),
  living_smoney NUMERIC(12,2),
  lsi_total_money_in_ai_money NUMERIC(12,2),
  lsi_total_money NUMERIC(12,2),
  dis_money NUMERIC(12,2),
  ad_money1 NUMERIC(12,2),
  ad_money2 NUMERIC(12,2),
  ac_ttl_point INTEGER NOT NULL,
  me_ttl_money NUMERIC(12,2),
  tax_in_me_ttl_money NUMERIC(12,2),
  oe_etc_ttl_money_non_taxable NUMERIC(12,2),
  oe_etc_ttl_money_taxable NUMERIC(12,2),
  tax_in_oe_etc_ttl_money_taxable NUMERIC(12,2),
  lsi_fv_money NUMERIC(12,2),
  lsi_sv_money NUMERIC(12,2),
  lsi_mm_money NUMERIC(12,2),
  lsi_other_money NUMERIC(12,2),
  ml_cost NUMERIC(12,2),
  meal_cost NUMERIC(12,2),
  living_cost NUMERIC(12,2),
  oe_meal_cost NUMERIC(12,2),
  oe_meal_smoney NUMERIC(12,2),
  oe_living_cost NUMERIC(12,2),
  oe_living_smoney NUMERIC(12,2),
  room_charge NUMERIC(12,2),
  tax_in_room_charge NUMERIC(12,2),
  _created_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  _updated_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE orca_income_ac_point(
  _id UUID PRIMARY KEY DEFAULT uuidv7(),
  _parent_id UUID NOT NULL REFERENCES orca_income(_id) ON DELETE CASCADE,
  _list_index INTEGER NOT NULL,
  ac_point_code VARCHAR(3) NOT NULL,
  ac_point_name VARCHAR(100) NOT NULL,
  ac_point INTEGER, -- NULL if no data corresponding to this category
  me_money NUMERIC(12,2),
  UNIQUE (_parent_id, _list_index)
);

CREATE TABLE orca_income_oe_etc(
  _id UUID PRIMARY KEY DEFAULT uuidv7(),
  _parent_id UUID NOT NULL REFERENCES orca_income(_id) ON DELETE CASCADE,
  _list_index INTEGER NOT NULL,
  oe_etc_number VARCHAR(10) NOT NULL,
  oe_etc_name VARCHAR(100) NOT NULL,
  oe_etc_money_non_taxable NUMERIC(12,2),
  oe_etc_money_taxable NUMERIC(12,2),
  UNIQUE (_parent_id, _list_index)
);

CREATE TABLE orca_income_insurance(
  _id UUID PRIMARY KEY DEFAULT uuidv7(),
  _parent_id UUID NOT NULL REFERENCES orca_income(_id) ON DELETE CASCADE,
  _list_index INTEGER NOT NULL,
  insurance_combination_number SMALLINT NOT NULL,
  insurance_provider_class CHAR(3),
  insurance_provider_number VARCHAR(8),
  insurance_provider_name VARCHAR(40),
  -- HealthInsuredPerson_Symbol 記号 (PII)
  -- HealthInsuredPerson_Number 番号 (PII)
  -- HealthInsuredPerson_Branch_Number 枝 (PII)
  UNIQUE (_parent_id, _list_index)
);

CREATE TABLE orca_income_public_insurance(
  _id UUID PRIMARY KEY DEFAULT uuidv7(),
  _parent_id UUID NOT NULL REFERENCES orca_income_insurance(_id) ON DELETE CASCADE,
  _list_index INTEGER NOT NULL,
  public_insurance_class VARCHAR(3) NOT NULL,
  public_insurance_name VARCHAR(20) NOT NULL,
  public_insurer_number VARCHAR(8) NOT NULL,
  -- PublicInsuredPerson_Number 受給者番号 (PII)
  UNIQUE (_parent_id, _list_index)
);

CREATE TABLE orca_income_unpaid(
  _id UUID PRIMARY KEY DEFAULT uuidv7(),
  _parent_id UUID NOT NULL REFERENCES orca_income(_id) ON DELETE CASCADE,
  _list_index INTEGER NOT NULL,
  perform_date DATE NOT NULL,
  inout CHAR(1) NOT NULL,
  invoice_number INTEGER NOT NULL,
  unpaid_money NUMERIC(12,2) NOT NULL,
  UNIQUE (_parent_id, _list_index)
);

COMMENT ON TABLE orca_income IS '収納情報API POST /api01rv2/incomeinfv2';
COMMENT ON TABLE orca_income_ac_point IS 'Ac_Point_Detail 点数詳細';
COMMENT ON TABLE orca_income_oe_etc IS 'Oe_Etc_Detail その他自費詳細';
COMMENT ON TABLE orca_income_insurance IS 'Insurance_Information 保険組合せ詳細';
COMMENT ON TABLE orca_income_public_insurance IS 'PublicInsurance_Information 公費情報';
COMMENT ON TABLE orca_income_unpaid IS 'Unpaid_Money_Information 個別の未収金情報. NOTE: these are unpaid invoices at the time of the API call, not the unpaid invoices at the time of the parent invoice data.';

COMMENT ON COLUMN orca_income.patient_id IS '患者番号';
COMMENT ON COLUMN orca_income.birthdate IS '生年月日';
COMMENT ON COLUMN orca_income.sex IS '性別';
COMMENT ON COLUMN orca_income.income_information_overflow IS '請求情報オーバーフラグ';

COMMENT ON COLUMN orca_income.perform_date IS '外来：診療日/入院：請求開始日';
COMMENT ON COLUMN orca_income.perform_end_date IS '請求終了日';
COMMENT ON COLUMN orca_income.issueddate IS '伝票発行日';
COMMENT ON COLUMN orca_income.inout IS '入外区分';
COMMENT ON COLUMN orca_income.invoice_number IS '伝票番号';
COMMENT ON COLUMN orca_income.group_invoice_number IS 'まとめ伝票番号';
COMMENT ON COLUMN orca_income.insurance_combination_number IS '保険組合せ番号';
COMMENT ON COLUMN orca_income.rate_cd IS '負担割合';
COMMENT ON COLUMN orca_income.department_code IS '診療科コード';
COMMENT ON COLUMN orca_income.department_name IS '診療科名称';
COMMENT ON COLUMN orca_income.ac_money IS '請求金額';
COMMENT ON COLUMN orca_income.tax_in_ac_money IS '請求金額消費税再掲';
COMMENT ON COLUMN orca_income.ic_money IS '入金額';
COMMENT ON COLUMN orca_income.ai_money IS '保険適用金額';
COMMENT ON COLUMN orca_income.oe_money IS '自費金額';
COMMENT ON COLUMN orca_income.dg_smoney IS '薬剤一部負担金';
COMMENT ON COLUMN orca_income.om_smoney IS '老人一部負担金';
COMMENT ON COLUMN orca_income.pi_smoney IS '公費一部負担金';
COMMENT ON COLUMN orca_income.ml_smoney IS '食事・生活療養負担金';
COMMENT ON COLUMN orca_income.meal_smoney IS '食事療養負担金';
COMMENT ON COLUMN orca_income.living_smoney IS '生活療養負担金';
COMMENT ON COLUMN orca_income.lsi_total_money_in_ai_money IS '保険適用金額内労災診察等合計金額';
COMMENT ON COLUMN orca_income.lsi_total_money IS '労災合計金額';
COMMENT ON COLUMN orca_income.dis_money IS '減免金額';
COMMENT ON COLUMN orca_income.ad_money1 IS '調整金１';
COMMENT ON COLUMN orca_income.ad_money2 IS '調整金２';
COMMENT ON COLUMN orca_income.ac_ttl_point IS '合計点数';
COMMENT ON COLUMN orca_income.me_ttl_money IS '保険適用外合計金額';
COMMENT ON COLUMN orca_income.tax_in_me_ttl_money IS '保険適用外合計金額消費税再掲';
COMMENT ON COLUMN orca_income.oe_etc_ttl_money_non_taxable IS 'その他自費（非課税分）合計金額';
COMMENT ON COLUMN orca_income.oe_etc_ttl_money_taxable IS 'その他自費（課税分）合計金額';
COMMENT ON COLUMN orca_income.tax_in_oe_etc_ttl_money_taxable IS 'その他自費（課税分）合計金額消費税再掲';
COMMENT ON COLUMN orca_income.lsi_fv_money IS '初診';
COMMENT ON COLUMN orca_income.lsi_sv_money IS '再診';
COMMENT ON COLUMN orca_income.lsi_mm_money IS '指導';
COMMENT ON COLUMN orca_income.lsi_other_money IS 'その他';
COMMENT ON COLUMN orca_income.ml_cost IS '食事・生活療養費';
COMMENT ON COLUMN orca_income.meal_cost IS '食事療養費';
COMMENT ON COLUMN orca_income.living_cost IS '生活療養費';
COMMENT ON COLUMN orca_income.oe_meal_cost IS '食事療養費（自費）';
COMMENT ON COLUMN orca_income.oe_meal_smoney IS '生活療養費（自費）';
COMMENT ON COLUMN orca_income.oe_living_cost IS '食事療養負担金（自費）';
COMMENT ON COLUMN orca_income.oe_living_smoney IS '生活療養負担金（自費）';
COMMENT ON COLUMN orca_income.room_charge IS '室料差額';
COMMENT ON COLUMN orca_income.tax_in_room_charge IS '室料差額消費税再掲';

COMMENT ON COLUMN orca_income_ac_point.ac_point_code IS '識別コード';
COMMENT ON COLUMN orca_income_ac_point.ac_point_name IS '名称';
COMMENT ON COLUMN orca_income_ac_point.ac_point IS '点数';
COMMENT ON COLUMN orca_income_ac_point.me_money IS '保険適用外金額';

COMMENT ON COLUMN orca_income_oe_etc.oe_etc_number IS '番号';
COMMENT ON COLUMN orca_income_oe_etc.oe_etc_name IS '項目名';
COMMENT ON COLUMN orca_income_oe_etc.oe_etc_money_non_taxable IS '非課税金額';
COMMENT ON COLUMN orca_income_oe_etc.oe_etc_money_taxable IS '課税金額';

COMMENT ON COLUMN orca_income_insurance.insurance_combination_number IS '保険組合せ番号';
COMMENT ON COLUMN orca_income_insurance.insurance_provider_class IS '保険の種類';
COMMENT ON COLUMN orca_income_insurance.insurance_provider_number IS '保険者番号';
COMMENT ON COLUMN orca_income_insurance.insurance_provider_name IS '保険の制度名称';

COMMENT ON COLUMN orca_income_public_insurance.public_insurance_class IS '公費の種類';
COMMENT ON COLUMN orca_income_public_insurance.public_insurance_name IS '公費の制度名称';
COMMENT ON COLUMN orca_income_public_insurance.public_insurer_number IS '負担者番号';

COMMENT ON COLUMN orca_income_unpaid.perform_date IS '診療日';
COMMENT ON COLUMN orca_income_unpaid.inout IS '入外区分';
COMMENT ON COLUMN orca_income_unpaid.invoice_number IS '伝票番号';
COMMENT ON COLUMN orca_income_unpaid.unpaid_money IS '未収金額';

CREATE INDEX idx_orca_income ON orca_income(perform_date, patient_id, invoice_number, department_code);

CREATE TABLE orca_patient_medical(
  _id UUID PRIMARY KEY DEFAULT uuidv7(),
  patient_id VARCHAR(20) NOT NULL,
  -- WholeName 患者氏名 (PII)
  -- WholeName_inKana 患者カナ氏名 (PII)
  birthdate DATE NOT NULL,
  sex orca_sex NOT NULL,
  perform_date DATE NOT NULL,
  department_code CHAR(2) NOT NULL,
  department_name VARCHAR(100) NOT NULL,
  sequential_number SMALLINT NOT NULL,
  _created_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  _updated_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE orca_patient_medical_history(
  _id UUID PRIMARY KEY DEFAULT uuidv7(),
  _parent_id UUID NOT NULL REFERENCES orca_patient_medical(_id) ON DELETE CASCADE,
  _list_index INTEGER NOT NULL,
  insurance_combination_number SMALLINT NOT NULL,
  insurance_provider_class CHAR(3),
  insurance_provider_number VARCHAR(8),
  insurance_provider_name VARCHAR(40),
  -- HealthInsuredPerson_Symbol 記号 (PII)
  -- HealthInsuredPerson_Number 番号 (PII)
  -- HealthInsuredPerson_Branch_Number 枝 (PII)
  invoice_number INTEGER, -- NULLABLE since 外来のみ
  physician_code VARCHAR(5), -- NULLABLE since 外来のみ
  -- Physician_WholeName ドクター名 (PII)
  UNIQUE (_parent_id, _list_index)
);

CREATE TABLE orca_patient_medical_public_insurance(
  _id UUID PRIMARY KEY DEFAULT uuidv7(),
  _parent_id UUID NOT NULL REFERENCES orca_patient_medical_history(_id) ON DELETE CASCADE,
  _list_index INTEGER NOT NULL,
  public_insurance_class VARCHAR(3) NOT NULL,
  public_insurance_name VARCHAR(20) NOT NULL,
  public_insurer_number VARCHAR(8) NOT NULL,
  -- PublicInsuredPerson_Number 受給者番号 (PII)
  UNIQUE (_parent_id, _list_index)
);

CREATE TABLE orca_patient_medical_item(
  _id UUID PRIMARY KEY DEFAULT uuidv7(),
  _parent_id UUID NOT NULL REFERENCES orca_patient_medical_history(_id) ON DELETE CASCADE,
  _list_index INTEGER NOT NULL,
  medical_class CHAR(3) NOT NULL,
  medical_class_name VARCHAR(100) NOT NULL,
  medical_class_number INTEGER,
  medical_class_point INTEGER,
  medical_class_money NUMERIC(12,2),
  medical_class_code CHAR(1),
  medical_inclusion_class BOOLEAN,
  medical_examination_count SMALLINT,
  patient_choice_point INTEGER,
  patient_choice_money NUMERIC(12,2),
  UNIQUE (_parent_id, _list_index)
);

CREATE TABLE orca_patient_medical_medication(
  _id UUID PRIMARY KEY DEFAULT uuidv7(),
  _parent_id UUID NOT NULL REFERENCES orca_patient_medical_item(_id) ON DELETE CASCADE,
  _list_index INTEGER NOT NULL,
  medication_code CHAR(9) NOT NULL,
  medication_name VARCHAR(200) NOT NULL,
  medication_name_input_value VARCHAR(200),
  medication_number NUMERIC(10,5),
  unit_code CHAR(3),
  unit_code_name VARCHAR(24),
  medication_input_code VARCHAR(20),
  medication_point_class SMALLINT,
  medication_point NUMERIC(11,2),
  medication_refer_point NUMERIC(11,2),
  UNIQUE (_parent_id, _list_index)
);

COMMENT ON TABLE orca_patient_medical IS '診療情報API class=02 POST /api01rv2/medicalgetv2?class=02';
COMMENT ON TABLE orca_patient_medical_history IS 'Medical_List_Information 受診履歴情報';
COMMENT ON TABLE orca_patient_medical_public_insurance IS 'PublicInsurance_Information 公費情報';
COMMENT ON TABLE orca_patient_medical_item IS 'Medical_Information 診療内容剤情報';
COMMENT ON TABLE orca_patient_medical_medication IS 'Medication_info 診療行為詳細';

COMMENT ON COLUMN orca_patient_medical.patient_id IS '患者番号';
COMMENT ON COLUMN orca_patient_medical.birthdate IS '生年月日';
COMMENT ON COLUMN orca_patient_medical.sex IS '性別';
COMMENT ON COLUMN orca_patient_medical.perform_date IS '診療年月日';
COMMENT ON COLUMN orca_patient_medical.department_code IS '診療科コード';
COMMENT ON COLUMN orca_patient_medical.department_name IS '診療科名称';
COMMENT ON COLUMN orca_patient_medical.sequential_number IS '連番';

COMMENT ON COLUMN orca_patient_medical_history.insurance_combination_number IS '保険組合せ番号';
COMMENT ON COLUMN orca_patient_medical_history.insurance_provider_class IS '保険の種類';
COMMENT ON COLUMN orca_patient_medical_history.insurance_provider_number IS '保険者番号';
COMMENT ON COLUMN orca_patient_medical_history.insurance_provider_name IS '保険の制度名称';
COMMENT ON COLUMN orca_patient_medical_history.invoice_number IS '伝票番号 外来のみ';
COMMENT ON COLUMN orca_patient_medical_history.physician_code IS 'ドクターコード 外来のみ';

COMMENT ON COLUMN orca_patient_medical_public_insurance.public_insurance_class IS '公費の種類';
COMMENT ON COLUMN orca_patient_medical_public_insurance.public_insurance_name IS '公費の制度名称';
COMMENT ON COLUMN orca_patient_medical_public_insurance.public_insurer_number IS '負担者番号';

COMMENT ON COLUMN orca_patient_medical_item.medical_class IS '診療種別区分';
COMMENT ON COLUMN orca_patient_medical_item.medical_class_name IS '診療種別区分名称';
COMMENT ON COLUMN orca_patient_medical_item.medical_class_number IS '回数';
COMMENT ON COLUMN orca_patient_medical_item.medical_class_point IS '剤点数';
COMMENT ON COLUMN orca_patient_medical_item.medical_class_money IS '剤金額';
COMMENT ON COLUMN orca_patient_medical_item.medical_class_code IS '剤区分';
COMMENT ON COLUMN orca_patient_medical_item.medical_inclusion_class IS '包括剤区分';
COMMENT ON COLUMN orca_patient_medical_item.medical_examination_count IS '包括検査項目数';
COMMENT ON COLUMN orca_patient_medical_item.patient_choice_point IS '長期収載品選定療養点数';
COMMENT ON COLUMN orca_patient_medical_item.patient_choice_money IS '長期収載品選定療養特別料金(税込)';

COMMENT ON COLUMN orca_patient_medical_medication.medication_code IS 'コード';
COMMENT ON COLUMN orca_patient_medical_medication.medication_name IS '名称';
COMMENT ON COLUMN orca_patient_medical_medication.medication_name_input_value IS 'コメント入力値';
COMMENT ON COLUMN orca_patient_medical_medication.medication_number IS '数量';
COMMENT ON COLUMN orca_patient_medical_medication.unit_code IS '単位';
COMMENT ON COLUMN orca_patient_medical_medication.unit_code_name IS '単位名称';
COMMENT ON COLUMN orca_patient_medical_medication.medication_input_code IS 'コメント埋め込み値';
COMMENT ON COLUMN orca_patient_medical_medication.medication_point_class IS '点数識別';
COMMENT ON COLUMN orca_patient_medical_medication.medication_point IS '点数';
COMMENT ON COLUMN orca_patient_medical_medication.medication_refer_point IS '参考点数';

CREATE INDEX idx_orca_patient_medical_date ON orca_patient_medical(perform_date, department_code);
CREATE INDEX idx_orca_patient_medical_history_invoice ON orca_patient_medical_history(invoice_number) NULLS NOT DISTINCT;

CREATE TABLE orca_patient_visit(
  _id UUID PRIMARY KEY DEFAULT uuidv7(),
  visit_date DATE NOT NULL,
  patient_id VARCHAR(20) NOT NULL,
  -- WholeName 漢字氏名 (PII)
  -- WholeName_inKana カナ氏名 (PII)
  birthdate DATE NOT NULL,
  sex orca_sex NOT NULL,
  department_code CHAR(2) NOT NULL,
  department_name VARCHAR(100) NOT NULL,
  physician_code VARCHAR(5),
  -- Physician_WholeName ドクター名 (PII)
  invoice_number INTEGER, -- NOTE: voucher_number in the original API
  sequential_number SMALLINT NOT NULL,
  insurance_combination_number SMALLINT NOT NULL,
  insurance_provider_class CHAR(3),
  insurance_provider_number VARCHAR(8),
  insurance_provider_name VARCHAR(40),
  -- HealthInsuredPerson_Symbol 記号 (PII)
  -- HealthInsuredPerson_Number 番号 (PII)
  -- HealthInsuredPerson_Branch_Number 枝 (PII)
  update_date DATE NOT NULL,
  update_time TIMETZ NOT NULL,
  patient_update_date DATE NOT NULL,
  patient_update_time TIMETZ NOT NULL,
  _created_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  _updated_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  UNIQUE(visit_date, department_code, patient_id, invoice_number, sequential_number)
);

CREATE TABLE orca_patient_visit_public_insurance(
  _id UUID PRIMARY KEY DEFAULT uuidv7(),
  _parent_id UUID NOT NULL REFERENCES orca_patient_visit(_id) ON DELETE CASCADE,
  _list_index INTEGER NOT NULL,
  public_insurance_class VARCHAR(3) NOT NULL,
  public_insurance_name VARCHAR(20) NOT NULL,
  public_insurer_number VARCHAR(8) NOT NULL,
  -- PublicInsuredPerson_Number 受給者番号 (PII)
  UNIQUE (_parent_id, _list_index)
);

COMMENT ON TABLE orca_patient_visit IS '来院患者一覧API POST /api01rv2/visitptlstv2';
COMMENT ON TABLE orca_patient_visit_public_insurance IS 'PublicInsurance_Information 公費情報';

COMMENT ON COLUMN orca_patient_visit.visit_date IS '来院日付';
COMMENT ON COLUMN orca_patient_visit.patient_id IS '患者番号';
COMMENT ON COLUMN orca_patient_visit.birthdate IS '生年月日';
COMMENT ON COLUMN orca_patient_visit.sex IS '性別';
COMMENT ON COLUMN orca_patient_visit.department_code IS '診療科コード';
COMMENT ON COLUMN orca_patient_visit.department_name IS '診療科名称';
COMMENT ON COLUMN orca_patient_visit.physician_code IS 'ドクターコード';
COMMENT ON COLUMN orca_patient_visit.invoice_number IS '伝票番号 (voucher_number in API)';
COMMENT ON COLUMN orca_patient_visit.sequential_number IS '連番';
COMMENT ON COLUMN orca_patient_visit.insurance_combination_number IS '保険組合せ番号';
COMMENT ON COLUMN orca_patient_visit.insurance_provider_class IS '保険の種類';
COMMENT ON COLUMN orca_patient_visit.insurance_provider_number IS '保険者番号';
COMMENT ON COLUMN orca_patient_visit.insurance_provider_name IS '保険の制度名称';
COMMENT ON COLUMN orca_patient_visit.update_date IS '更新日付';
COMMENT ON COLUMN orca_patient_visit.update_time IS '更新時間';
COMMENT ON COLUMN orca_patient_visit.patient_update_date IS '患者情報更新日';
COMMENT ON COLUMN orca_patient_visit.patient_update_time IS '患者情報更新時間';

COMMENT ON COLUMN orca_patient_visit_public_insurance.public_insurance_class IS '公費の種類';
COMMENT ON COLUMN orca_patient_visit_public_insurance.public_insurance_name IS '公費の制度名称';
COMMENT ON COLUMN orca_patient_visit_public_insurance.public_insurer_number IS '負担者番号';

COMMIT;
