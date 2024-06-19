library(mockery)
library(lifelihood)

test_that("detect_os correctly identifies Windows", {
   mock_sys_info <- mock(c(sysname = "Windows"))
   stub(detect_os, "Sys.info", mock_sys_info)
   expect_equal(detect_os(), "Windows")
   expect_called(mock_sys_info, 1)
})

test_that("detect_os correctly identifies Linux", {
   mock_sys_info <- mock(c(sysname = "Linux"))
   stub(detect_os, "Sys.info", mock_sys_info)
   expect_equal(detect_os(), "Unix-like")
   expect_called(mock_sys_info, 1)
})

test_that("detect_os correctly identifies Darwin (macOS)", {
   mock_sys_info <- mock(c(sysname = "Darwin"))
   stub(detect_os, "Sys.info", mock_sys_info)
   expect_equal(detect_os(), "Unix-like")
   expect_called(mock_sys_info, 1)
})

test_that("detect_os returns 'Unknown' for other systems", {
   mock_sys_info <- mock(c(sysname = "Solaris"))
   stub(detect_os, "Sys.info", mock_sys_info)
   expect_equal(detect_os(), "Unknown")
   expect_called(mock_sys_info, 1)
})
