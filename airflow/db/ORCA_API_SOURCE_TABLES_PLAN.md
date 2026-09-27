# ORCA API source table plan

## 収納API

API path:
- `POST /api01rv2/incomeinfv2`

API tables:
- `orca_income`
- `orca_income_ac_point`
- `orca_income_oe_etc`
- `orca_income_insurance`
- `orca_income_public_insurance`
- `orca_income_unpaid`

## 診療情報API class=02

API path:
- `POST /api01rv2/medicalgetv2?class=02`

API tables:
- `orca_patient_medical`
- `orca_patient_medical_history`
- `orca_patient_medical_item`
- `orca_patient_medical_medication`
- `orca_patient_medical_public_insurance`

## 来院患者一覧API

API path:
- `POST /api01rv2/visitptlstv2`

API tables:
- `orca_patient_visit`
- `orca_patient_visit_public_insurance`
