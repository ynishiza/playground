---
NOTE:
  This is a managed document.
  The content should be precise and succinct, in RFC style.
  No verbose essays.
  This document is intended to help both human readers and guide AI agents.
  An AI agent MUST NOT automatically edit this file.
---

# Contents

- [ORCA API source tables](#orca-api-source-tables)
  - [Metadata](#metadata)
  - [Model: nested object](#model-nested-object)
  - [Model: nested list](#model-nested-list)
  - [Data types](#data-types)
  - [IMPORTANT: PII](#important-pii)

# ORCA API source tables

Main resources:
- [ORCA API reference](https://www.orca.med.or.jp/receipt/users/tec/api/overview.html)
- [ORCA specification files](https://www.orca.med.or.jp/receipt/users/tec/index.html)

Target files:
- `db/orca_api.sql`


General:
- Model each API response as a SQL `TABLE`.\
  See [nested object](#model-nested-object) and [nested list](#model-nested-list)

- Naming: use a descriptive table name for each API prefixed with `orca_`.\
  e.g. 1. 患者基本情報 API: `orca_patient` \
  e.g. 27. 収納情報API: `orca_income`

- Model: exclude metadata columns from the API response.\
  Common columns:
  ```
  Information_Date
  Information_Time
  Api_Result
  Api_Result_Message
  Reskey
  ```

- Model: **NEVER** add indexes by default.

- Documentation
  - API table: document each API table with the API name and API path.\
    e.g. 患者情報API
    ```
    COMMENT ON TABLE orca_patient IS '患者基本情報 GET /api01rv2/patientgetv2';
    ```

  - API field: document each column corresponding to an API field as a column comment.\
    e.g. 患者情報API
    ```
    COMMENT ON COLUMN orca_patient.patient_id IS '患者番号';
    COMMENT ON COLUMN orca_patient.birth_date IS '生年月日';
    ```

  - nested lists: document each [nested list](#model-nested-list) as a table comment with the list name in the API.\
    e.g. 収納情報API's Income_Information (請求情報) list.
    ```
    COMMENT ON TABLE orca_income_information IS 'Income_Information 請求情報';
    ```

[Top](#contents)

## Metadata

Metadata columns are prefixed with `_`.

- system ID:
  Each table MUST have a `_id` defined at the top.\
  Use this as the default primary key.\
  Use this ID for foreign keys i.e. `REFERENCES`.

  ```
  _id UUID PRIMARY KEY DEFAULT uuidv7()
  ```

- system dates:
  Each top table MUST have created and update dates defined as the very last columns of the table.

  ```
  _created_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  _updated_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
  ```

[Top](#contents)

## Model: nested object

In general, flatten nested objects in the table.\
i.e. DO NOT normalize out to a separate table.

Some common objects:

1. Patient_Information 患者情報

    Object in API spec:

    ```
    Patient_ID　患者番号
    WholeName　患者氏名
    WholeName_inKana 患者カナ氏名
    BirthDate　生年月日
    Sex　性別
    ```

    Corresponding SQL data model:

    ```
    patient_id VARCHAR(20) NOT NULL
    -- WholeName 患者氏名 (PII)
    -- WholeName_inKana 患者カナ氏名 (PII)
    birthdate DATE NOT NULL
    sex orca_sex NOT NULL
    ```

2. HealthInsurance_Information 保険組合せ情報

    Object in API spec:

    ```
    Insurance_Combination_Number 保険組合せ番号
    HealthInsurance_Information 保険組合せ情報
      InsuranceProvider_Class 保険の種類
      InsuranceProvider_WholeName 保険の制度名称
      InsuranceProvider_Number 保険者番号
      HealthInsuredPerson_Symbol 記号
      HealthInsuredPerson_Number 番号
      HealthInsuredPerson_Branch_Number 枝
      PublicInsurance_Information 公費情報
        PublicInsurance_Class 公費の種類
        PublicInsurance_Name 公費の制度名称
        PublicInsurer_Number 負担者番号
        PublicInsuredPerson_Number 受給者番号
    ```

    Corresponding SQL data model:\
    a) Base model

    NOTE: these values may be NULL if the patient has no insurance.

    ```
    insurance_combination_number SMALLINT NOT NULL,
    insurance_provider_class CHAR(3),
    insurance_provider_number VARCHAR(8),
    insurance_provider_name VARCHAR(40),
    -- HealthInsuredPerson_Symbol 記号 (PII)
    -- HealthInsuredPerson_Number 番号 (PII)
    -- HealthInsuredPerson_Branch_Number 枝 (PII)
    health_insured_person_branch_number VARCHAR(2),
    ```

    b) Public insurance list (公費情報) modeled as a separate list table:

    ```
    public_insurance_class VARCHAR(3) NOT NULL,
    public_insurance_name VARCHAR(20) NOT NULL,
    public_insurer_number VARCHAR(8) NOT NULL,
    -- PublicInsuredPerson_Number 受給者番号 (PII)
    ```

[Top](#contents)

## Model: nested list

Model a list in the API object as a separate table.\
Include a `_list_index` to indicate the position of the item in the list.\
Associate list table with the parent table using a foreign key `_parent_id` referencing the parent's `_id`.

e.g.収納情報API's Income_Information (請求情報) list

```
CREATE TABLE orca_income_information(
  _id INT NOT NULL,
  _parent_id UUID NOT NULL REFERENCES orca_patient(_id) ON DELETE CASCADE,
  _list_index INTEGER NOT NULL,
  ...

  UNIQUE(_parent_id, _list_index)
)
```


[Top](#contents)

## Data types

Reference:

- [ORCA DB schema](../documents/ORCA/database-table-definition-edition-20240426.pdf)

General rules
- NEVER use `UUID` for API fields.
- NEVER use any auto-generated values for API fields.
- custom type: sex values\
  Define a custom type:
  ```
  CREATE TYPE orca_sex AS ENUM ('M', 'F');
  ```

  Use `M` for `sex=1` and `F` for `sex=2`.


Common fields:

- Patient
  - 患者番号 (max 20 chars): `patient_id VARCHAR(20) NOT NULL`
  - 患者ID (10 digits): `patient_internal_id BIGINT NOT NULL`
  - 性別: `sex orca_sex`
  - 郵便番号: `VARCHAR(7)`
- 診療科：`department_code CHAR(2) NOT NULL`
- 診療科名：`department_name VARCHAR(100) NOT NULL`
- 医師コード：`physician_code VARCHAR(5) NOT NULL`
- 連番 (2 digits)：`sequential_number SMALLINT NOT NULL`
- Income
  - 伝票番号 (7 digits)：`invoice_number INTEGER NOT NULL`
  - 金額：`*_money NUMERIC(12,2)`
- Medication
  - 診療区分（診療行為区分）: `medical_class CHAR(2) NOT NULL`
  - 診療種別区分： `medical_class CHAR(3) NOT NULL`
  - 診療行為コード (9 digits): `medication_code CHAR(9) NOT NULL`
  - 剤点数： `medical_class_point INTEGER NOT NULL`
  - 回数・剤回数：`medical_class_number INTEGER`
  - 点数(点or金額)： `medication_point NUMERIC(11,2)`
  - 数量： `medication_number NUMERIC(10,5)`
  - 単位：`unit_code CHAR(3)`
  - 単位名称：`unit_code_name VARCHAR(24)`



[Top](#contents)

## IMPORTANT: PII

**NEVER** include PII fields in the source table definitions.\
Instead, include an inline `--` comment with the API field name at the same position in the table definition, as a PII placeholder for documentation.\
e.g.

```
patient_id VARCHAR(20) NOT NULL,
-- WholeName 患者氏名 (PII)
-- WholeName_inKana 患者カナ氏名 (PII)
```

PII fields:

- Names of people: `WholeName`, `WholeName_inKana`
- `HealthInsuredPerson_Symbol`, `HealthInsuredPerson_Number`, `HealthInsuredPerson_Branch_Number`
- `PublicInsuredPerson_Number`
- `EmailAddress`
- Physical addresses: `WholeAddress`
- Phone numbers: `PhoneNumber`, `CellularNumber`, `FaxNumber`

[Top](#contents)
