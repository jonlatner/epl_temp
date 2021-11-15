# Top commands ----
# Create empty R application (no figures, data frames, packages, etc.)
# https://stackoverflow.com/questions/7505547/detach-all-packages-while-working-in-r
detachAllPackages <- function() {
        basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
        package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
        package.list <- setdiff(package.list,basic.packages)
        if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
        
}
detachAllPackages()

rm(list=ls(all=TRUE))

# load library
library(tidyverse)
library(OECD)
library(countrycode)

# FOLDERS - ADAPT THIS PATHWAY
setwd("/Users/jonathanlatner/Documents/GitHub/epl_temp/")
graphs = "graphs/"

# Load data ----

# https://www.oecd.org/els/emp/oecdindicatorsofemploymentprotection.htm
df_epl_p <- get_dataset("EPL_OV") # https://stats.oecd.org/Index.aspx?DataSetCode=EPL_OV
df_epl_t <- get_dataset("EPL_T") # https://stats.oecd.org/Index.aspx?DataSetCode=EPL_T
df_temp_0 <- get_dataset("TEMP_I") # https://stats.oecd.org/Index.aspx?DataSetCode=TEMP_I

# Code European country/region ----

country_name <- c("Germany", "Switzerland", "Luxembourg", "Belgium", "Austria", "Netherlands", "France", 
                  "United Kingdom", "Ireland", 
                  "Malta", "Greece", "Italy", "Cyprus", "Portugal", "Spain", 
                  "Romania", "Poland", "Croatia", "Hungary", "Czechia", "Bulgaria", "Slovenia", "Slovakia", "Lithuania", "Estonia", "Latvia", "Serbia", "Turkey",
                  "Sweden", "Denmark", "Norway", "Finland", "Iceland")
region <- c("Continental", "Continental", "Continental", "Continental", "Continental", "Continental", "Continental", 
            "Anglophone", "Anglophone", 
            "Southern", "Southern", "Southern", "Southern", "Southern", "Southern", 
            "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern",
            "Nordic", "Nordic", "Nordic", "Nordic", "Nordic")

geography <- cbind(country_name, region)
rm(country_name, region)

# Clean EPL data ----

df_epl <- rbind(df_epl_p,df_epl_t)

names(df_epl) <- tolower(names(df_epl))

df_epl <- df_epl %>%
        filter(series == "EPRC_V1" | series == "EPT_V1") %>%
        select(country,obstime,obsvalue,series) %>%
        rename(year=obstime,
               epl=obsvalue) %>%
        mutate(year = as.numeric(year)) %>%
        mutate(series = ifelse(series == "EPRC_V1", yes = "perm_epl", no = "temp_epl")) %>%
        filter(year>=1990)

# Estimate EPL gap, similar to Passaretta and Wolbers, 2019
# According to footnote 9, negatives values are imputed to 0
# Yet more than 25% of all values are less than 0
# JPL Comment: This seems to be bad science

df_epl_diff <- pivot_wider(df_epl,names_from = series,values_from = epl)
df_epl_diff$epl_gap <- with(df_epl_diff,perm_epl-temp_epl)
df_epl_diff <- df_epl_diff[complete.cases(df_epl_diff),]

# Clean Temp data ----

names(df_temp_0) <- tolower(names(df_temp_0))

df_temp_1 <- df_temp_0 %>%
        filter(sex == "MW", # men and women
               series == "PER_CENT_TEMP", # percent temporary
               age == "1524", # young workers
        ) %>%
        select(-series) %>%
        rename(year=obstime,
               temp=obsvalue) %>%
        mutate(year = as.numeric(year)) %>%
        filter(year>=1990)

# Merge data ----

df_merge <- merge(df_epl, df_temp_1, by = c("country","year"), all.x = TRUE)
df_merge$country_name <- countrycode(df_merge$country, 'genc3c', 'country.name')
df_merge <- merge(df_merge,geography,all.y = TRUE)
df_merge <- df_merge %>%
        arrange(country,series,year)

df_merge_diff <- merge(df_epl_diff, df_temp_1, by = c("country","year"), all.x = TRUE)
df_merge_diff$country_name <- countrycode(df_merge_diff$country, 'genc3c', 'country.name')
df_merge_diff <- merge(df_merge_diff,geography,all.y = TRUE)
df_merge_diff <- df_merge_diff %>%
        arrange(country,year)

# Clean data ----

df_merge <- df_merge %>%
        filter(!is.na(temp)) %>% # countries must have temporary employment rate
        group_by(country,series) %>%
        mutate(sum = row_number(),
               sum = last(sum)) %>%
        mutate(cor = round(cor(epl,temp),3)) %>% # estimate correlation
        ungroup() %>%
        filter(sum>2) %>% # countries must have at least 3 observations
        select(-sum)
df_merge <- droplevels(df_merge)

df_merge_diff <- df_merge_diff %>%
        filter(!is.na(temp)) %>% # countries must have temporary employment rate
        group_by(country) %>%
        mutate(sum = row_number(),
               sum = last(sum)) %>%
        mutate(cor = round(cor(epl_gap,temp),3)) %>% # estimate correlation
        ungroup() %>%
        filter(sum>2) %>% # countries must have at least 3 observations
        select(-sum)
df_merge_diff <- droplevels(df_merge_diff)

# Graph ----

# Graphs the relationship between EPL (temp) and temporary employment
df_graph <- df_merge %>%
        filter(series == "temp_epl")

ggplot(data = df_graph, aes(x = year)) +
        facet_wrap(~region+country_name,scales = "free_y") +
        geom_line(aes(y=epl, color = "EPL")) +
        geom_line(aes(y=temp/10, color = "Temp emp")) +
        theme_bw() +
        scale_color_manual(values = c("blue", "orange")) +
        scale_y_continuous(
                name = "EPL (Temporary employment)",
                sec.axis = sec_axis(~./10, name="Temporary employment (%)")
        ) +
        geom_text(aes(label=paste("r = ", cor)), 
                  size = 3, 
                  x=-Inf, y=Inf, hjust=-0.2, vjust=1.2)+
        theme(panel.grid.minor = element_blank(), 
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5),
              legend.title=element_blank(),
              legend.key.width = unit(2,"cm"),
              legend.position = "bottom"
        )
ggsave(filename = paste0(graphs,"graph_eplT_tempY.pdf"), plot = last_plot(), height = 8, width = 12, units = "in")

# Graphs the relationship between EPL (perm) and temporary employment
df_graph <- df_merge %>%
        filter(series == "perm_epl")

ggplot(data = df_graph, aes(x = year)) +
        facet_wrap(~region+country_name,scales = "free_y") +
        geom_line(aes(y=epl, color = "EPL")) +
        geom_line(aes(y=temp/10, color = "Temp emp")) +
        theme_bw() +
        scale_color_manual(values = c("blue", "orange")) +
        scale_y_continuous(
                name = "EPL (Permanent employment)",
                sec.axis = sec_axis(~./10, name="Temporary employment (%)")
        ) +
        geom_text(aes(label=paste("r = ", cor)),
                  size = 3, 
                  x=-Inf, y=Inf, hjust=-0.2, vjust=1.2)+
        theme(panel.grid.minor = element_blank(), 
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5),
              legend.title=element_blank(),
              legend.key.width = unit(2,"cm"),
              legend.position = "bottom"
        )
ggsave(filename = paste0(graphs,"graph_eplP_tempY.pdf"), plot = last_plot(), height = 8, width = 12, units = "in")

# Graphs the relationship between EPL (gap) and temporary employment
df_graph <- df_merge_diff

ggplot(data = df_graph, aes(x = year)) +
        facet_wrap(~region+country_name,scales = "free_y") +
        geom_line(aes(y=epl_gap, color = "EPL gap")) +
        geom_line(aes(y=temp/10, color = "Temp emp")) +
        theme_bw() +
        scale_color_manual(values = c("blue", "orange")) +
        scale_y_continuous(
                name = "EPL gap",
                sec.axis = sec_axis(~./10, name="Temporary employment (%)")
        ) +
        geom_text(aes(label=paste("r = ", cor)), 
                  size = 3, 
                  x=-Inf, y=Inf, hjust=-0.2, vjust=1.2)+
        theme(panel.grid.minor = element_blank(), 
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5),
              legend.title=element_blank(),
              legend.key.width = unit(2,"cm"),
              legend.position = "bottom"
        )
ggsave(filename = paste0(graphs,"graph_eplG_tempY.pdf"), plot = last_plot(), height = 8, width = 12, units = "in")


