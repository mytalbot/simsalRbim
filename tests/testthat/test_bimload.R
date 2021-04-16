library(simsalRbim)
context("Testing ZickeZacke data")

test_that("Checks if the ZickeZacke data can be loaded", {
  expect_equal(dim(ZickeZacke)[1], 30)
  expect_equal(dim(ZickeZacke)[2], 5)
})





test_that("Tests the preprocess function", {
  expect_equal(dim(bimpre (dat=ZickeZacke, GT=c("Zacke","Huehner","Kacke" ),
                           simOpt="Zicke", deviation=0, minQuantity=0,
                           verbose=FALSE))[1],24)

  expect_equal(dim(bimpre (dat=ZickeZacke, GT=c("Zicke","Huehner","Kacke" ),
                           simOpt="Zacke", deviation=0, minQuantity=0,
                           verbose=FALSE))[1],24)

  expect_equal(names(bimpre (dat=ZickeZacke, GT=c("Zacke","Huehner","Kacke" ),
                           simOpt="Zicke", deviation=0, minQuantity=0,
                           verbose=FALSE)),
               c("subjectID","optionA","optionB","quantityA","quantityB","test",
                 "result","qPercentA",
                 "qPercentB","tie","sim","optionFactorA","optionFactorB"))

  expect_equal(names(bimpre (dat=ZickeZacke, GT=c("Zicke","Huehner","Kacke" ),
                           simOpt="Zacke", deviation=0, minQuantity=0,
                           verbose=FALSE)),
               c("subjectID","optionA","optionB","quantityA","quantityB","test",
                 "result","qPercentA",
                 "qPercentB","tie","sim","optionFactorA","optionFactorB"))

  expect_equal(sum(bimpre (dat=ZickeZacke, GT=c("Zicke","Zacke","Huehner",
                                                "Kacke" ),
                           simOpt="HoiHoiHoi", deviation=0, minQuantity=0,
                           verbose=FALSE)$tie),  11)

  expect_equal(sum(bimpre (dat=ZickeZacke,
                           GT=c("Zicke","Zacke","Huehner","Kacke" ),
                           simOpt="HoiHoiHoi", deviation=50, minQuantity=0,
                           verbose=FALSE)$tie), 40)
})



test_that("Tests the worth function", {
  expect_equal( class(bimworth(ydata    = bimpre (dat=ZickeZacke,
                                                  GT=c("Zacke","Huehner","Kacke" ),
                                                  simOpt="Zicke",
                                                  verbose=FALSE),
                               GT       = c("Zacke","Huehner","Kacke" ),
                               simOpt   = "Zicke",
                               showPlot = FALSE))[1], "wmat")

})








