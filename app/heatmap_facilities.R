setwd("~/GitHub/Spr2017-proj2-proj2_grp8/data/City Raw")

library(choroplethrZip)
library(dtplyr)
library(dplyr)
library(DT)
library(lubridate)
library(shiny)

## Facilities
# 1. Library
# NYC
Library.NYC=read.csv('NYC/Library.csv')
Library.NYC=
  Library.NYC%>%
  filter(Library.NYC$ZIP>0)%>%
  mutate(region=as.character(ZIP))
Library.NYC=
  Library.NYC%>%
  group_by(region)%>%
  summarise(
    value=n()
  )
zip_choropleth(Library.NYC,
               title       = "Library in NYC",
               legend      = "Number of Libraries",
               zip_zoom = Library.NYC$region)

# Chicago
Library.Chicago=read.csv('Chicago/Library.csv')
Library.Chicago=
  Library.Chicago%>%
  filter(Library.Chicago$ZIP>0)%>%
  mutate(region=as.character(ZIP))
Library.Chicago=
  Library.Chicago%>%
  group_by(region)%>%
  summarise(
    value=n()
  )
zip_choropleth(Library.Chicago,
               title       = "Library in Chicago",
               legend      = "Number of Libraries",
               zip_zoom = Library.Chicago$region)

# Austin
Library.Austin=read.csv('Austin/Library.csv')
Library.Austin=
  Library.Austin%>%
  filter(Library.Austin$Zip>0)%>%
  mutate(region=as.character(Zip))
Library.Austin=
  Library.Austin%>%
  group_by(region)%>%
  summarise(
    value=n()
  )
zip_choropleth(Library.Austin,
               title       = "Library in Austin",
               legend      = "Number of Libraries",
               zip_zoom = Library.Austin$region)

# LA
Library.LA=read.csv('LA/Library.csv')
Library.LA=
  Library.LA%>%
  filter(Library.LA$Zip.Code>0)%>%
  mutate(region=as.character(Zip.Code))
Library.LA=
  Library.LA%>%
  group_by(region)%>%
  summarise(
    value=n()
  )
zip_choropleth(Library.LA,
               title       = "Library in Los Angeles",
               legend      = "Number of Library",
               zip_zoom = Library.LA$region)

# SF
Library.SF=read.csv('San Franciso/Library.csv')
Library.SF=
  Library.SF%>%
  filter(Library.SF$Zip.Code>0)%>%
  mutate(region=as.character(Zip.Code))
Library.SF=
  Library.SF%>%
  group_by(region)%>%
  summarise(
    value=n()
  )
zip_choropleth(Library.SF,
               title       = "Library in San Francisco",
               legend      = "Number of Libraries",
               zip_zoom = Library.SF$region)

# 2. Park
# NYC
Park.NYC=read.csv('NYC/Park.csv')
Park.NYC=
  Park.NYC%>%
  filter(Park.NYC$Zip.Code>0)%>%
  mutate(region=as.character(Zip.Code))
Park.NYC=
  Park.NYC%>%
  group_by(region)%>%
  summarise(
    value=n()
  )
zip_choropleth(Park.NYC,
               title       = "Park in NYC",
               legend      = "Number of Parks",
               zip_zoom = Park.NYC$region)

# Chicago
Park.Chicago=read.csv('Chicago/Park.csv')
Park.Chicago=
  Park.Chicago%>%
  filter(Park.Chicago$ZIP>0)%>%
  mutate(region=as.character(ZIP))
Park.Chicago=
  Park.Chicago%>%
  group_by(region)%>%
  summarise(
    value=n()
  )
Park.Chicago=Park.Chicago[-25,]
Park.Chicago=Park.Chicago[-32,]
zip_choropleth(Park.Chicago,
               title       = "Park in Chicago",
               legend      = "Number of Parks",
               zip_zoom = Park.Chicago$region)

# Austin
Park.Austin=read.csv('Austin/Park.csv')
Park.Austin=
  Park.Austin%>%
  filter(Park.Austin$ZIP_CODE>0)%>%
  mutate(region=as.character(ZIP_CODE))
Park.Austin=
  Park.Austin%>%
  group_by(region)%>%
  summarise(
    value=n()
  )
zip_choropleth(Park.Austin,
               title       = "Park in Austin",
               legend      = "Number of Parks",
               zip_zoom = Park.Austin$region)

# LA
Park.LA=read.csv('LA/Park.csv')
Park.LA=
  Park.LA%>%
  filter(Park.LA$Zip>0)%>%
  mutate(region=as.character(Zip))
Park.LA=
  Park.LA%>%
  group_by(region)%>%
  summarise(
    value=n()
  )
zip_choropleth(Park.LA,
               title       = "Park in Los Angeles",
               legend      = "Number of Parks",
               zip_zoom = Park.LA$region)

# SF
Park.SF=read.csv('San Franciso/Park.csv')
Park.SF=
  Park.SF%>%
  filter(Park.SF$Zipcode>0)%>%
  mutate(region=as.character(Zipcode))
Park.SF=
  Park.SF%>%
  group_by(region)%>%
  summarise(
    value=n()
  )
zip_choropleth(Park.SF,
               title       = "Park in San Francisco",
               legend      = "Number of Parks",
               zip_zoom = Park.SF$region)

# 3. Health Care
# NYC
Health.NYC=read.csv('NYC/Health.csv')
Health.NYC=
  Health.NYC%>%
  filter(Health.NYC$Zip>0)%>%
  mutate(region=as.character(Zip))
Health.NYC=
  Health.NYC%>%
  group_by(region)%>%
  summarise(
    value=n()
  )
zip_choropleth(Health.NYC,
               title       = "Health Care in NYC",
               legend      = "Number of Health Care",
               zip_zoom = Health.NYC$region)

# Chicago
Health.Chicago=read.csv('Chicago/Health.csv')
Health.Chicago=
  Health.Chicago%>%
  filter(Health.Chicago$Zip>0)%>%
  mutate(region=as.character(Zip))
Health.Chicago=
  Health.Chicago%>%
  group_by(region)%>%
  summarise(
    value=n()
  )
Health.Chicago=Health.Chicago[-37,]
zip_choropleth(Health.Chicago,
               title       = "Health Care in Chicago",
               legend      = "Number of Health Care",
               zip_zoom = Health.Chicago$region)

# Austin
Health.Austin=read.csv('Austin/Health.csv')
Health.Austin=
  Health.Austin%>%
  filter(Health.Austin$Zip.Code>0)%>%
  mutate(region=as.character(Zip.Code))
Health.Austin=
  Health.Austin%>%
  group_by(region)%>%
  summarise(
    value=n()
  )
zip_choropleth(Health.Austin,
               title       = "Health Care in Austin",
               legend      = "Number of Health Care",
               zip_zoom = Health.Austin$region)

# LA
Health.LA=read.csv('LA/Health.csv')
Health.LA=
  Health.LA%>%
  filter(Health.LA$Zip.Code>0)%>%
  mutate(region=as.character(Zip.Code))
Health.LA=
  Health.LA%>%
  group_by(region)%>%
  summarise(
    value=n()
  )
zip_choropleth(Health.LA,
               title       = "Health Care in Log Angeles",
               legend      = "Number of Health Care",
               zip_zoom = Health.LA$region)

# SF
Health.SF=read.csv('San Franciso/Health.csv')
Health.SF=
  Health.SF%>%
  filter(Health.SF$Zip>0)%>%
  mutate(region=as.character(Zip))
Health.SF=
  Health.SF%>%
  group_by(region)%>%
  summarise(
    value=n()
  )
Health.SF=Health.SF[-19,]
zip_choropleth(Health.SF,
               title       = "Health Care in Log Angeles",
               legend      = "Number of Health Care",
               zip_zoom = Health.SF$region)

