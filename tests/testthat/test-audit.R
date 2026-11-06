test_that('audit flags ambiguous experimental units and missingness', {
  x <- data.frame(material=c('A','A','B'),age_hours=c(0,24,200),frequency_hz=c(10,100,10),
                  permittivity=c(1,2,3),dissipation_factor=c(1,1,1),impedance=c(1,NA,3))
  a <- audit_dielectric(x)
  expect_equal(a$summary$rows,3)
  expect_equal(a$missingness$missing[a$missingness$variable=='impedance'],1)
  expect_true(any(grepl('168-hour',a$warnings)))
  expect_true(any(grepl('NOT confirmed',a$warnings)))
})
