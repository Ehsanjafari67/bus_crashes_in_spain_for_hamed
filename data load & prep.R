#----------------------------------library--------------------------------------
library(vtable)
library(caret)
library(janitor)
library(tidyverse)
library(scales)
library(lubridate)
library(tidymodels)
library(themis)
library(baguette)
#---------------------------------load and filter data--------------------------
#| cache: TRUE
load("RawData.RData")

bus_crash <- MGE_drv_acc_veh |> 
  filter(VEHICLE_TYPE == 15 | VEHICLE_TYPE == 16 | VEHICLE_TYPE == 17)
#
save(bus_crash,
     file = "BUS_CRASH.RData")

load("BUS_CRASH.RData")
#----------------------------------names modification---------------------------
#| results: hide
class(bus_crash)
bus_crash_raw <- as_tibble(bus_crash)  
class(bus_crash_raw)
names(bus_crash_raw)

bus_crash_raw <- bus_crash_raw |> 
  clean_names("upper_camel", abbreviations = c("ID", "KM"))
names(bus_crash_raw)
#----------------------------------Near Zero Variables--------------------------
#| eval: false
(nzv <- nearZeroVar(bus_crash_raw, saveMetrics= TRUE))
dim(bus_crash_raw)
nzv <- nearZeroVar(bus_crash_raw)
names(bus_crash_raw[nzv])

bus_crash_raw <- bus_crash_raw[, -nzv]
dim(bus_crash_raw)
sumtable(bus_crash_raw)
names(bus_crash_raw)
#----------------------------------select Variables-----------------------------
#| cache: TRUE
crash <- bus_crash_raw |>  
  arrange(desc(AccidentDate))  |> 
  transmute(injuries = if_else(TotalInjMore24H30D > 0, "injuries", "none"),
            Age,
            Sex,
            CertifYear,
            BeltUse,
            PlannedTravelDistance,
            Violation,
            ViolSpeeding,
            FactorAffectAttention,
            Weekdays,
            RoadType,
            TotalVehicles,
            AccTypeCollision,
            SurfCondition,
            LightningCondition,
            Visibility,
            RoadFunction,
            Speed,
            SpeedLimit,     
            LaneWidth,
            SeparateMarking,
            SeparateMedian,
            SeparateGuardrail,
            SeparateNone,
            SectionBridge,
            SectionTunnel,
            NarrowSection,
            SectionSpeedBumpUp,
            SectionNone,
            MarginTrees,
            MarginBuilds,
            MarginPosts,
            MarginUnknown,
            SpecialConConst,
            RoadDelimCurb,
            RoadDelimMark,
            RoadDelimBarrier,
            RoadDelimNone,
            DistractedDrive,
            NoRespectPriority,
            Alcohol,
            VehicleAge,
            TechTestItv,
            PrevProblemNone,
            PrevProblemTires,
            PrevProblemSteering,
            PrevProblemBrakes,
            NumOccupants,
            ContinDrivHours,
            MostDamage,
            )
str(crash)
sumtable(crash)
#--------------------------------Variables identifying--------------------------
#| cache: TRUE
df <- crash
df[df == 998 | df == 999] <- NA

for (col in names(df)) {
  uniq_val <- unique(df[[col]])
  n_uniq <- length(uniq_val)
  n_miss <- sum(is.na(df[[col]]))
  if (n_uniq < 100) {
    print(paste("Column:", col, "- Number of unique values:", n_uniq))
    print(paste("Column:", col, "- Number of missing values:", n_miss))
    tbl <- table(df[[col]])
    print(paste("Column:", col, "- Ordered Frequency Table:"))
    print(tbl[order(tbl, decreasing = TRUE)])
  } 
}

crash <- df

to be continue.....