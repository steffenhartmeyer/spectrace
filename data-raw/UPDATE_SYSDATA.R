data.path = file.path("data-raw" ,"data")

calibration = readr::read_csv(file.path(data.path, "spectrace_calibration.csv"))
calibration.old = readr::read_csv(file.path(data.path, "spectrace_calibration_old.csv"))
spectrace_responses_1nm = readr::read_csv(file.path(data.path, "spectrace_responses_1nm.csv"))
reference_spectra_1nm = readr::read_csv(file.path(data.path, "reference_spectra_1nm.csv"))
cie_illuminants_1nm = readr::read_csv(file.path(data.path, "CIE_illuminants_1nm.csv"))
cie_s26e_1nm = readr::read_csv(file.path(data.path, "CIEs026_1nm.csv"))
cie_xyz_1nm = readr::read_csv(file.path(data.path, "CIE_xyz_1nm.csv"))
v_lambda_1nm = readr::read_csv(file.path(data.path, "V_lambda_1nm.csv"))
cla_1nm = readr::read_csv(file.path(data.path, "CLA_1nm.csv" ))

usethis::use_data(
  calibration,
  calibration.old,
  spectrace_responses_1nm,
  reference_spectra_1nm,
  cie_illuminants_1nm,
  cie_s26e_1nm,
  cie_xyz_1nm,
  v_lambda_1nm,
  cla_1nm,
  internal = TRUE,
  overwrite = TRUE
)
