# Hollywood Closet Project:

## *Exploratory Data Analysis*

<br>

------------------------------------------------------------------------

## Overview

##### <span style="color: dimgray;">*I worked on collecting this dataset as an RA in my undergrad. I decided to do a more in-depth analysis to see what insights may be gained about the data.*</span>


##### <span style="color: dimgray;">*Because the sample was non-randomly selected and was not designed to comply with the theoretical assumptions of statistical tests, I decided to not include inferential procedures for lack of external validity. However, the descriptive statistics and visualizations provide fascinating insight into the sample. Further, these analyses provide an idea of how the population may look.*</span>


##### <span style="color: dimgray;">*Future endeavors should focus on further collection of data to provide a more representative sample to gain firmer insight.*</span>

## <span style="color: steelblue;">Research Questions</span>

-   What do the variables in this data set look like with descriptive statistics and data visualization?
-   How has the age of coming out changed over time?
-   Are people coming out at a younger age in more recent years?
-   How do all of these look across race, gender, and sexuality?

------------------------------------------------------------------------

## <span style="color: steelblue;">1</span> Load the data set

------------------------------------------------------------------------



```R
options(warn = -1) # Turn off warning messages

# Load required libraries
suppressMessages(library(tidyverse))
suppressMessages(library(wesanderson))
suppressMessages(library(forcats))
suppressMessages(library(knitr))
suppressMessages(library(kableExtra))
suppressMessages(library(ggrepel))
suppressMessages(library(patchwork))
```

<br>

### <span style="color: lightgray;">1.1</span> Read in the file



```R
data <- read.csv("HCP_data.csv", na.strings = c("","NA"))
```

<br>

### <span style="color: lightgray;">1.2</span> Preview the `data`



```R
head(data)
```


<table class="dataframe">
<caption>A data.frame: 6 × 15</caption>
<thead>
	<tr><th></th><th scope=col>ID</th><th scope=col>First.Name</th><th scope=col>Last.Name</th><th scope=col>Race.Ethnicity</th><th scope=col>Birth.Year</th><th scope=col>Coming.Out.Age</th><th scope=col>Coming.Out.Year</th><th scope=col>Coming.Out.Medium</th><th scope=col>Type.of.Outing.Initiation</th><th scope=col>Type.of.Outing.Who.discloses.</th><th scope=col>Public.Sexual.Identity</th><th scope=col>Were.they.in..the.closet..</th><th scope=col>Public.Gender.Identity</th><th scope=col>X2nd.Coming.Out..Nature</th><th scope=col>X2nd.Coming.Out..Sexual.ID</th></tr>
	<tr><th></th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th></tr>
</thead>
<tbody>
	<tr><th scope=row>1</th><td>1</td><td>Jackie      </td><td>Abbott        </td><td>White </td><td>1992</td><td>30</td><td>2017</td><td>Social Media</td><td>Other initiated</td><td>Other's words</td><td>Unspecified</td><td>Unclear</td><td>Cis Woman</td><td>NA       </td><td>NA     </td></tr>
	<tr><th scope=row>2</th><td>2</td><td>Ali         </td><td>Adler         </td><td>White </td><td>1967</td><td>20</td><td>1987</td><td>Print       </td><td>Self initiated </td><td>Own words    </td><td>Lesbian    </td><td>Unclear</td><td>Cis Woman</td><td>Amplified</td><td>Lesbian</td></tr>
	<tr><th scope=row>3</th><td>3</td><td>James       </td><td>Adomian       </td><td>White </td><td>1980</td><td>30</td><td>2010</td><td>Online      </td><td>Other initiated</td><td>Other's words</td><td>Gay        </td><td>Yes    </td><td>Cis Man  </td><td>NA       </td><td>NA     </td></tr>
	<tr><th scope=row>4</th><td>4</td><td>Roberto     </td><td>Aguirre Sacasa</td><td>Latinx</td><td>1973</td><td>31</td><td>2004</td><td>Print       </td><td>Other initiated</td><td>Other's words</td><td>Gay        </td><td>No     </td><td>Cis Man  </td><td>NA       </td><td>NA     </td></tr>
	<tr><th scope=row>5</th><td>5</td><td>Clay        </td><td>Aiken         </td><td>White </td><td>1978</td><td>29</td><td>2007</td><td>Print       </td><td>Self initiated </td><td>Own words    </td><td>Gay        </td><td>Unclear</td><td>Cis Man  </td><td>NA       </td><td>NA     </td></tr>
	<tr><th scope=row>6</th><td>6</td><td>Loretta Mary</td><td>Aiken         </td><td>Black </td><td>1894</td><td>27</td><td>1921</td><td>NA          </td><td>NA             </td><td>NA           </td><td>Unspecified</td><td>NA     </td><td>Cis Woman</td><td>Amplified</td><td>Lesbian</td></tr>
</tbody>
</table>



<br>

------------------------------------------------------------------------

## <span style="color: steelblue;">2 </span>Data Wrangling and Cleaning

------------------------------------------------------------------------

<br>

###  <span style="color: lightgrey;">2.1</span> Relabel columns


```R
colnames(data) = c("id", "first_name", "last_name", "race", "birth_year", "age", 
                  "coming_out_year", "medium", "initiation", "disclosure", "sexuality", 
                  "closeted", "gender", "coming_out_nature_2", "sexuality_2")
```

<br>

### <span style="color: lightgrey;">2.2</span> Merge `first_name` and `last_name`


```R
data <- data %>%
  unite("name", first_name:last_name, sep=" ")
```

<br>

### <span style="color: lightgrey;">2.3</span> Convert variables to correct data types



```R
# All character variables to factors
data <- data %>%
  mutate_if(sapply(data, is.character), as.factor)

```

<br>

### <span style="color: lightgrey;">2.4</span> Create variable `coming_out_2` to easily explore subset



```R
data<- data %>% 
  mutate(coming_out_2 = ifelse(is.na(coming_out_nature_2), 0, 1) ,.after = gender)

```

<br>

------------------------------------------------------------------------
 
## <span style="color: steelblue;">3</span> Data Visualization and Summary

------------------------------------------------------------------------

<br>

## <span style="color: lightgrey;">3.1</span> Univariate Analysis

<br>

------------------------------------------------------------------------

### <span style="color: steelblue;">Race</span> 



```R
options(repr.plot.width=12, repr.plot.height=8) #Set plot size 


pct_format <- scales::percent_format(accuracy = .1) #Used in the future bar plots

data %>%
  filter(race != "Unknown") %>%
  ggplot(aes(x = fct_infreq(race), fill = fct_infreq(race))) +
  geom_bar() +
  guides(fill = "none") +
  geom_text(
    aes(label = sprintf(
      "%d (%s)", ..count.., pct_format(..count.. / sum(..count..))
    )),
    stat = "count",
    nudge_y = 5,
    colour = "black",
    size = 4,
    fontface = "bold"
  ) +
  labs(title = "Figure 1.1", x = "", y = "Count") +
  theme_classic() +
  theme(plot.title = element_text(color = "black",
                                  size = 14,
                                  face = "bold")) +
  theme(axis.text.x = element_text(size = 12, vjust = -1)) +
  scale_fill_manual(values = wes_palette("Moonrise3", 5, type = "continuous"))
```


    
![png](output_15_0.png)
    






```R
data %>%
  filter(!is.na(race)) %>%
  group_by(race) %>%
  rename(Race = race) %>%
  summarise(Frequency = n()) %>%
  mutate("Relative Frequency" = round(Frequency / sum(Frequency), 3)) %>%
  kable(caption = "TABLE 1.1: Race",        
        format = "html",
        table.attr = "style='width:65%;'") %>%
  kable_classic() %>%
  kable_styling(bootstrap_options = c("striped"), font = 18) %>%
  row_spec(0, bold = TRUE) %>%
  as.character() %>%
  IRdisplay::display_html()

```


<table style='width:65%; font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto; font-size: 18px; margin-left: auto; margin-right: auto;' class=" lightable-classic table table-striped">
<caption style="font-size: initial !important;">TABLE 1.1: Race</caption>
 <thead>
  <tr>
   <th style="text-align:left;font-weight: bold;"> Race </th>
   <th style="text-align:right;font-weight: bold;"> Frequency </th>
   <th style="text-align:right;font-weight: bold;"> Relative Frequency </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Asian </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 0.080 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Black </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 0.159 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Latinx </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 0.125 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Multiracial </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 0.068 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Unknown </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.006 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> White </td>
   <td style="text-align:right;"> 99 </td>
   <td style="text-align:right;"> 0.562 </td>
  </tr>
</tbody>
</table>


<br>

#### <span style="color: gray;">Any missing values?</span>



```R
# Create function to calculate return # of NAs in a variable
NA_calc <- function(x) {
  a <- sum(is.na(x))
  print(paste("Missing Values (NAs):" , a))
}
# Calculate NAs
NA_calc(data$race)
```

    [1] "Missing Values (NAs): 0"


<br>

------------------------------------------------------------------------

### <span style="color: steelblue;">Gender</span>



```R
options(repr.plot.width=10, repr.plot.height=8) #Set plot size 

data %>%
  filter(!is.na(gender)) %>%
  ggplot(aes(x = fct_infreq(gender), fill = fct_infreq(gender))) +
  geom_bar() +
  guides(fill = "none") +
  geom_text(
    aes(label = sprintf(
      "%d (%s)", ..count.., pct_format(..count.. / sum(..count..))
    )),
    stat = "count",
    nudge_y = 5,
    colour = "black",
    size = 3.5,
    fontface = "bold"
  ) +
  labs(title = "Figure 1.2", x = "", y = "Count") +
  theme_classic() +
  theme(plot.title = element_text(color = "black",
                                  size = 14,
                                  face = "bold")) +
  theme(axis.text.x = element_text(
    size = 11,
    angle = 25,
    hjust = 1
  )) +
  scale_fill_manual(values = wes_palette("Moonrise3", 10, type = "continuous"))

```


    
![png](output_21_0.png)
    






```R
data %>%
  filter(!is.na(gender)) %>%
  group_by(gender) %>%
  rename(Gender = gender) %>%
  summarise(Frequency = n()) %>%
  mutate("Relative Frequency" = round(Frequency / sum(Frequency), 3)) %>%
  kable(caption = "TABLE 1.2: Gender",
        format = "html",
        table.attr = "style='width:65%;'") %>%
  kable_classic() %>%
  kable_styling(bootstrap_options = c("striped"), font = 18) %>%
  row_spec(0, bold = TRUE) %>%
  as.character() %>%
  IRdisplay::display_html()

```


<table style='width:65%; font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto; font-size: 18px; margin-left: auto; margin-right: auto;' class=" lightable-classic table table-striped">
<caption style="font-size: initial !important;">TABLE 1.2: Gender</caption>
 <thead>
  <tr>
   <th style="text-align:left;font-weight: bold;"> Gender </th>
   <th style="text-align:right;font-weight: bold;"> Frequency </th>
   <th style="text-align:right;font-weight: bold;"> Relative Frequency </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Cis Man </td>
   <td style="text-align:right;"> 76 </td>
   <td style="text-align:right;"> 0.434 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cis Woman </td>
   <td style="text-align:right;"> 65 </td>
   <td style="text-align:right;"> 0.371 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Genderqueer </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.006 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Non binary </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 0.040 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Queer </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0.011 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Trans Man </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 0.017 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Trans Woman </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 0.074 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Unspecified </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 0.040 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Unspecified queer </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.006 </td>
  </tr>
</tbody>
</table>


<br>

#### <span style="color: gray;">Any missing values?</span>



```R
NA_calc(data$gender)
```

    [1] "Missing Values (NAs): 1"


<br>

------------------------------------------------------------------------

### <span style="color: steelblue;">Sexuality</span>



```R
options(repr.plot.width=12, repr.plot.height=8) #Set plot size 

#The value "Unspecified non heterosexual" fits poorly, so I have revalued it to "Non heterosexual".
data %>%
  filter(!is.na(sexuality)) %>%
  mutate(sexuality = fct_recode(sexuality, "Non heterosexual" = "Unspecified non heterosexual")) %>%
  ggplot(aes(x = fct_infreq(sexuality), fill = fct_infreq(sexuality))) +
  geom_bar() +
  guides(fill = "none") +
  geom_text(
    aes(label = sprintf(
      "%d (%s)", ..count.., pct_format(..count.. / sum(..count..))
    )),
    stat = "count",
    nudge_y = 5,
    colour = "black",
    size = 3.5,
    fontface = "bold"
  ) +
  labs(title = "Figure 1.3", x = "", y = "Count") +
  theme_classic() +
  theme(plot.title = element_text(color = "black",
                                  size = 14,
                                  face = "bold")) +
  theme(axis.text.x = element_text(
    size = 12,
    angle = 35,
    hjust = 1.0
  )) +
  scale_fill_manual(values = wes_palette("Moonrise3", 9, type = "continuous"))

```


    
![png](output_27_0.png)
    






```R
data %>%
  filter(!is.na(sexuality)) %>%
  group_by(sexuality) %>%
  mutate(sexuality = fct_recode(sexuality, "Non heterosexual" = "Unspecified non heterosexual")) %>%
  rename(Sexuality = sexuality) %>%
  summarise(Frequency = n()) %>%
  mutate("Relative Frequency" = round(Frequency / sum(Frequency), 3)) %>%
  kable(caption = "TABLE 1.3: Sexuality",
        format = "html",
        table.attr = "style='width:60%;'") %>%
  kable_classic() %>%
  kable_styling(bootstrap_options = c("striped"), font = 18) %>%
  row_spec(0, bold = TRUE) %>%
  as.character() %>%
  IRdisplay::display_html()

```


<table style='width:60%; font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto; font-size: 18px; margin-left: auto; margin-right: auto;' class=" lightable-classic table table-striped">
<caption style="font-size: initial !important;">TABLE 1.3: Sexuality</caption>
 <thead>
  <tr>
   <th style="text-align:left;font-weight: bold;"> Sexuality </th>
   <th style="text-align:right;font-weight: bold;"> Frequency </th>
   <th style="text-align:right;font-weight: bold;"> Relative Frequency </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Bisexual </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 0.119 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Gay </td>
   <td style="text-align:right;"> 64 </td>
   <td style="text-align:right;"> 0.381 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Heterosexual </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 0.030 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Lesbian </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 0.125 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pansexual </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.006 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Queer </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 0.077 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sexually fluid </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.006 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Unspecified </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 0.179 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Non heterosexual </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 0.077 </td>
  </tr>
</tbody>
</table>


<br>

#### <span style="color: gray;">Any missing values?</span>



```R
NA_calc(data$sexuality)
```

    [1] "Missing Values (NAs): 8"


<br>

------------------------------------------------------------------------

### <span style="color: steelblue;">In the closet?</span>



```R
data %>%
  filter(!is.na(closeted)) %>%
  ggplot(aes(x = closeted, fill = closeted)) +
  geom_bar() +
  guides(fill = "none") +
  geom_text(
    aes(label = sprintf(
      "%d (%s)", ..count.., pct_format(..count.. / sum(..count..))
    )),
    stat = "count",
    nudge_y = 5,
    colour = "black",
    size = 3.5,
    fontface = "bold"
  ) +
  labs(title = "Figure 1.4", x = "", y = "Count") +
  theme_classic() +
  theme(plot.title = element_text(color = "black",
                                  size = 14,
                                  face = "bold")) +
  theme(axis.text.x = element_text(size=12, angle = 35, hjust = 1)) +
  scale_fill_manual(values = c("#85D4E3","LightBlue", "Linen", "#F4B5BD", "MistyRose"))

```


    
![png](output_33_0.png)
    






```R
data %>%
  filter(!is.na(closeted)) %>%
  group_by(closeted) %>%
  rename("Closeted?" = closeted) %>%
  summarise(Frequency = n()) %>%
  mutate("Relative Frequency" = round(Frequency/sum(Frequency), 3)) %>%
  kable(caption = "TABLE 1.4: Closeted before coming out", 
        format = "html", 
        table.attr = "style='width:65%;'") %>% 
  kable_classic() %>%
  kable_styling(bootstrap_options = c("striped"), font = 18) %>%
  row_spec(0,bold=TRUE) %>%
  as.character() %>%
  IRdisplay::display_html()

```


<table style='width:65%; font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto; font-size: 18px; margin-left: auto; margin-right: auto;' class=" lightable-classic table table-striped">
<caption style="font-size: initial !important;">TABLE 1.4: Closeted before coming out</caption>
 <thead>
  <tr>
   <th style="text-align:left;font-weight: bold;"> Closeted? </th>
   <th style="text-align:right;font-weight: bold;"> Frequency </th>
   <th style="text-align:right;font-weight: bold;"> Relative Frequency </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> No </td>
   <td style="text-align:right;"> 66 </td>
   <td style="text-align:right;"> 0.398 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> No but not publicly out </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 0.018 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Unclear </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:right;"> 0.271 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:right;"> 0.301 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Yes but rumored </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0.012 </td>
  </tr>
</tbody>
</table>


<br>

#### <span style="color: gray;">Any missing values?</span>



```R
NA_calc(data$closeted)

```

    [1] "Missing Values (NAs): 10"


<br>

------------------------------------------------------------------------

### <span style="color: steelblue;">Coming Out Medium</span>



```R
options(repr.plot.width=12, repr.plot.height=8) #Set plot size 

data %>%
  filter(!is.na(medium)) %>%
  ggplot(aes(x = fct_infreq(medium), fill = fct_infreq(medium))) +
  geom_bar() +
  guides(fill = "none") +
  geom_text(
    aes(label = sprintf(
      "%d (%s)", ..count.., pct_format(..count.. / sum(..count..))
    )),
    stat = "count",
    nudge_y = 5,
    colour = "black",
    size = 3.5,
    fontface = "bold"
  ) +
  labs(title = "Figure 1.5", x = "", y = "Count") +
  theme_classic() +
  theme(plot.title = element_text(color = "black",
                                  size = 14,
                                  face = "bold")) +
  theme(axis.text.x = element_text(size=11, angle=35, hjust = 1)) +
  scale_fill_manual(values = wes_palette("Moonrise3", 6, type = "continuous"))

```


    
![png](output_39_0.png)
    






```R
data %>%
  group_by(medium) %>%
  filter(!is.na(medium)) %>%
  rename(Medium = medium) %>%
  summarise(Frequency = n()) %>%
  mutate("Relative Frequency" = round(Frequency/sum(Frequency), 3)) %>%
  kable(caption = "TABLE 1.5: Medium of coming out",
        format = "html", 
        table.attr = "style='width:65%;'") %>% 
  kable_classic() %>%
  kable_styling(bootstrap_options = c("striped"), font = 18) %>%
  row_spec(0,bold=TRUE) %>%
  as.character() %>%
  IRdisplay::display_html()

```


<table style='width:65%; font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto; font-size: 18px; margin-left: auto; margin-right: auto;' class=" lightable-classic table table-striped">
<caption style="font-size: initial !important;">TABLE 1.5: Medium of coming out</caption>
 <thead>
  <tr>
   <th style="text-align:left;font-weight: bold;"> Medium </th>
   <th style="text-align:right;font-weight: bold;"> Frequency </th>
   <th style="text-align:right;font-weight: bold;"> Relative Frequency </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Article </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.006 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Broadcast Media </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 0.161 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Misc </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 0.071 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Online </td>
   <td style="text-align:right;"> 51 </td>
   <td style="text-align:right;"> 0.304 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Print </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 0.238 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Social Media </td>
   <td style="text-align:right;"> 37 </td>
   <td style="text-align:right;"> 0.220 </td>
  </tr>
</tbody>
</table>


<br>

#### <span style="color: gray;">Any missing values?</span>



```R
NA_calc(data$medium)

```

    [1] "Missing Values (NAs): 8"


<br>

------------------------------------------------------------------------

### <span style="color: steelblue;">Who Initiated Coming Out?</span>



```R
options(repr.plot.width=7, repr.plot.height=7) #Set plot size 

data %>%
  filter(!is.na(initiation)) %>%
  ggplot(aes(x = fct_infreq(initiation), fill = fct_infreq(initiation))) +
  geom_bar() +
  guides(fill = "none") +
  geom_text(
    aes(label = sprintf(
      "%d (%s)", ..count.., pct_format(..count.. / sum(..count..))
    )),
    stat = "count",
    nudge_y = 5,
    colour = "black",
    size = 4,
    fontface = "bold"
  ) +
  labs(title = "Figure 1.6: Initiation", x = "", y = "Count") +
  theme_classic() +
  theme(plot.title = element_text(color = "black",
                                  size = 14,
                                  face = "bold")) +
  theme(axis.text.x = element_text(size=12)) +
  scale_fill_manual(values = wes_palette("Moonrise3", 2, type = "continuous"))

```


    
![png](output_45_0.png)
    






```R
data %>%
  group_by(initiation) %>%
  filter(!is.na(initiation)) %>%
  rename(Initiation = initiation) %>%
  summarise(Frequency = n()) %>%
  mutate("Relative Frequency" = round(Frequency/sum(Frequency), 3)) %>%
  kable(caption = "TABLE 1.6: Who initiated coming out",
        format = "html", table.attr = "style='width:65%;'") %>% 
  kable_classic() %>%
  kable_styling(bootstrap_options = c("striped"), font = 18) %>%
  row_spec(0,bold=TRUE) %>%
  as.character() %>%
  IRdisplay::display_html()

```


<table style='width:65%; font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto; font-size: 18px; margin-left: auto; margin-right: auto;' class=" lightable-classic table table-striped">
<caption style="font-size: initial !important;">TABLE 1.6: Who initiated coming out</caption>
 <thead>
  <tr>
   <th style="text-align:left;font-weight: bold;"> Initiation </th>
   <th style="text-align:right;font-weight: bold;"> Frequency </th>
   <th style="text-align:right;font-weight: bold;"> Relative Frequency </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Other initiated </td>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:right;"> 0.247 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Self initiated </td>
   <td style="text-align:right;"> 125 </td>
   <td style="text-align:right;"> 0.753 </td>
  </tr>
</tbody>
</table>


<br>

#### <span style="color: gray;">Any missing values?</span>



```R
NA_calc(data$initiation)
```

    [1] "Missing Values (NAs): 10"


<br>

------------------------------------------------------------------------

### <span style="color: steelblue;">Who Disclosed?</span>



```R
data %>%
  filter(!is.na(disclosure)) %>%
  ggplot(aes(x = fct_infreq(disclosure), fill = fct_infreq(disclosure))) +
  geom_bar() +
  guides(fill = "none") +
  geom_text(
    aes(label = sprintf(
      "%d (%s)", ..count.., pct_format(..count.. / sum(..count..))
    )),
    stat = "count",
    nudge_y = 5,
    colour = "black",
    size = 4,
    fontface = "bold"
  ) +
  labs(title = "Figure 1.7: Disclosure", x = "", y = "Count") +
  theme_classic() +
  theme(plot.title = element_text(color = "black",
                                  size = 14,
                                  face = "bold")) +
  theme(axis.text.x = element_text(size=12)) +
  scale_fill_manual(values = wes_palette("Moonrise3", 2, type = "continuous"))

```


    
![png](output_51_0.png)
    






```R
data %>%
  group_by(disclosure) %>%
  filter(!is.na(disclosure)) %>%
  rename(Disclosure = disclosure) %>%
  summarise(Frequency = n()) %>%
  mutate("Relative Frequency" = round(Frequency/sum(Frequency), 3)) %>%
  kable(caption = "TABLE 1.7: Who disclosed", format = "html", table.attr = "style='width:65%;'") %>% 
  kable_classic() %>%
  kable_styling(bootstrap_options = c("striped"), font = 18) %>%
  row_spec(0,bold=TRUE) %>%
  as.character() %>%
  IRdisplay::display_html()

```


<table style='width:65%; font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto; font-size: 18px; margin-left: auto; margin-right: auto;' class=" lightable-classic table table-striped">
<caption style="font-size: initial !important;">TABLE 1.7: Who disclosed</caption>
 <thead>
  <tr>
   <th style="text-align:left;font-weight: bold;"> Disclosure </th>
   <th style="text-align:right;font-weight: bold;"> Frequency </th>
   <th style="text-align:right;font-weight: bold;"> Relative Frequency </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Other's words </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 0.188 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Own words </td>
   <td style="text-align:right;"> 134 </td>
   <td style="text-align:right;"> 0.812 </td>
  </tr>
</tbody>
</table>


<br>

#### <span style="color: gray;">Any missing values?</span>



```R
NA_calc(data$disclosure)

```

    [1] "Missing Values (NAs): 11"


<br>

------------------------------------------------------------------------

### <span style="color: steelblue;">Had Second Coming Out?</span>



```R
data %>%
  filter(!is.na(coming_out_2)) %>%
  mutate(coming_out_2 = as.factor(coming_out_2)) %>%
  mutate(coming_out_2 = if_else(coming_out_2 == "1", "Yes", "No")) %>%
  ggplot(aes(x = fct_infreq(coming_out_2), fill = fct_infreq(coming_out_2))) +
  geom_bar() +
  guides(fill = "none") +
  geom_text(
    aes(label = sprintf(
      "%d (%s)", ..count.., pct_format(..count.. / sum(..count..))
    )),
    stat = "count",
    nudge_y = 5,
    colour = "black",
    size = 4.5,
    fontface = "bold"
  ) +
  labs(title = "Figure 1.8", x = "", y = "Count") +
  theme_classic() +
  theme(plot.title = element_text(color = "black",
                                  size = 14,
                                  face = "bold")) +
  theme(axis.text.x = element_text(size=12)) +
  scale_fill_manual(values = wes_palette("Moonrise3", 2, type = "continuous"))

```


    
![png](output_57_0.png)
    






```R
data %>%
  group_by(coming_out_2) %>%
  filter(!is.na(coming_out_2)) %>%
  mutate(coming_out_2 = as.factor(coming_out_2)) %>%
  mutate(coming_out_2 = if_else(coming_out_2 == "1", "Yes", "No")) %>%
  rename("Had 2nd Coming Out" = coming_out_2) %>%
  summarise(Frequency = n()) %>%
  mutate("Relative Frequency" = round(Frequency/sum(Frequency), 2)) %>%
  kable(caption = "TABLE 1.8: Second coming out events", format = "html", table.attr = "style='width:65%;'") %>%
  kable_classic() %>%
  kable_styling(bootstrap_options = c("striped"), font = 18) %>%
  row_spec(0,bold=TRUE) %>%
  as.character() %>%
  IRdisplay::display_html()

```


<table style='width:65%; font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto; font-size: 18px; margin-left: auto; margin-right: auto;' class=" lightable-classic table table-striped">
<caption style="font-size: initial !important;">TABLE 1.8: Second coming out events</caption>
 <thead>
  <tr>
   <th style="text-align:left;font-weight: bold;"> Had 2nd Coming Out </th>
   <th style="text-align:right;font-weight: bold;"> Frequency </th>
   <th style="text-align:right;font-weight: bold;"> Relative Frequency </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> No </td>
   <td style="text-align:right;"> 131 </td>
   <td style="text-align:right;"> 0.74 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:right;"> 0.26 </td>
  </tr>
</tbody>
</table>


<br>

#### <span style="color: gray;">Any missing values?</span>



```R
NA_calc(data$coming_out_2)
```

    [1] "Missing Values (NAs): 0"


<br>

------------------------------------------------------------------------

### <span style="color: steelblue;">Second Coming Out Nature</span>



```R
data %>%
  filter(coming_out_2 == 1, 
         !is.na(coming_out_nature_2)) %>%
  ggplot(aes(x = fct_infreq(coming_out_nature_2), fill = fct_infreq(coming_out_nature_2))) +
  geom_bar() +
  guides(fill = "none") +
  geom_text(
    aes(label = sprintf(
      "%d (%s)", ..count.., pct_format(..count.. / sum(..count..))
    )),
    stat = "count",
    nudge_y = 1,
    colour = "black",
    size = 4.5,
    fontface = "bold"
  ) +
  labs(title = "Figure 1.9", x = "", y = "Count") +
  theme_classic() +
  theme(plot.title = element_text(color = "black",
                                  size = 14,
                                  face = "bold")) +
  theme(axis.text.x = element_text(size=12)) +
  scale_fill_manual(values = wes_palette("Moonrise3", 2, type = "continuous"))
```


    
![png](output_63_0.png)
    






```R
data %>%
  group_by(coming_out_nature_2) %>%
  filter(coming_out_2 == 1,
         !is.na(coming_out_nature_2)) %>%
  rename("Nature" = coming_out_nature_2) %>%
  summarise(Frequency = n()) %>%
  mutate("Relative Frequency" = round(Frequency/sum(Frequency), 2)) %>%
  kable(caption = "TABLE 1.9: Nature of second coming out", format = "html", table.attr = "style='width:65%;'") %>%
  kable_classic() %>%
  kable_styling(bootstrap_options = c("striped"), font = 18) %>%
  row_spec(0,bold=TRUE) %>%
  as.character() %>%
  IRdisplay::display_html()
```


<table style='width:65%; font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto; font-size: 18px; margin-left: auto; margin-right: auto;' class=" lightable-classic table table-striped">
<caption style="font-size: initial !important;">TABLE 1.9: Nature of second coming out</caption>
 <thead>
  <tr>
   <th style="text-align:left;font-weight: bold;"> Nature </th>
   <th style="text-align:right;font-weight: bold;"> Frequency </th>
   <th style="text-align:right;font-weight: bold;"> Relative Frequency </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Amplified </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 0.64 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> New </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:right;"> 0.36 </td>
  </tr>
</tbody>
</table>


<br>

#### <span style="color: gray;">Any missing values?</span>



```R
# Create subset of those with second comings out
subset <- data %>%
  filter(coming_out_2 == 1)

NA_calc(subset$coming_out_nature_2)
```

    [1] "Missing Values (NAs): 0"


<br>

------------------------------------------------------------------------

### <span style="color: steelblue;">Second Coming Out Sexuality</span>


```R
options(repr.plot.width=12, repr.plot.height=8) #Set plot size 

data %>%
  filter(coming_out_2 == 1, 
         !is.na(sexuality_2)) %>%
  mutate(sexuality_2 = fct_recode(sexuality_2,"Non heterosexual"= "Unspecified non heterosexual")) %>%
  ggplot(aes(x = fct_infreq(sexuality_2), fill = fct_infreq(sexuality_2))) +
  geom_bar() +
  guides(fill = "none") +
  geom_text(
    aes(label = sprintf(
      "%d (%s)", ..count.., pct_format(..count.. / sum(..count..))
    )),
    stat = "count",
    nudge_y = .65,
    colour = "black",
    size = 3.45,
    fontface = "bold"
  ) +
  labs(title = "Figure 1.10", x = "", y = "Count") +
  theme_classic() +
  theme(plot.title = element_text(color = "black",
                                  size = 14,
                                  face = "bold")) +
  theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1)) +
  scale_fill_manual(values = wes_palette("Moonrise3", 10, type = "continuous"))

```


    
![png](output_69_0.png)
    






```R
data %>%
  mutate(sexuality_2 = fct_recode(sexuality_2, "Non heterosexual"= "Unspecified non heterosexual",
                                  "Unspecified" = "Unspecified ")) %>%
  group_by(sexuality_2) %>%
  filter(coming_out_2 == 1,
         !is.na(sexuality_2)) %>%
  rename("Sexuality" = sexuality_2) %>%
  summarise(Frequency = n()) %>%
  mutate("Relative Frequency" = round(Frequency/sum(Frequency), 2)) %>%
  kable(caption = "TABLE 1.10: Sexuality at second coming out", format = "html", table.attr = "style='width:75%;'") %>%
  kable_classic() %>%
  kable_styling(bootstrap_options = c("striped"), font = 18) %>%
  row_spec(0,bold=TRUE) %>%
  as.character() %>%
  IRdisplay::display_html()

```


<table style='width:75%; font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto; font-size: 18px; margin-left: auto; margin-right: auto;' class=" lightable-classic table table-striped">
<caption style="font-size: initial !important;">TABLE 1.10: Sexuality at second coming out</caption>
 <thead>
  <tr>
   <th style="text-align:left;font-weight: bold;"> Sexuality </th>
   <th style="text-align:right;font-weight: bold;"> Frequency </th>
   <th style="text-align:right;font-weight: bold;"> Relative Frequency </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Bisexual </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0.05 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Gay </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 0.33 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Heterosexual </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.02 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Lesbian </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 0.14 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pansexual </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 0.07 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Queer </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 0.09 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sexually fluid </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.02 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Unspecified </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 0.21 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Non heterosexual </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 0.07 </td>
  </tr>
</tbody>
</table>


<br>

#### <span style="color: gray;">Any missing values?</span>



```R
NA_calc(subset$sexuality_2)
```

    [1] "Missing Values (NAs): 2"


<br>

### <span style="color: steelblue;">Current Age</span>

<br>

#### Current ages in 2022 based on birth year. For simplicity, I applied a filter to include only those under 100 years old



```R
options(repr.plot.width=12, repr.plot.height=8) #Set plot size 

p1 <- data %>%
  filter(!is.na(birth_year)) %>%
  mutate(current_age = 2022 - birth_year) %>%
  filter(current_age < 100) %>%
  ggplot(aes(x = current_age)) +
  geom_histogram(
    fill = "#85D4E3",
    colour = 1,
    binwidth = 5,
    size = .25
  ) +
  guides(fill = "none", colour = "none") +
  labs(title = "Figure 1.11",
       x = "Age",
       y = "Count") +
  coord_cartesian(xlim = c(10, 100), ylim = c(0, 40)) +
  theme_classic() +
  theme(plot.title = element_text(color = "black",
                                  size = 20,
                                  face = "bold"))

p2 <- data %>%
  filter(!is.na(birth_year)) %>%
  mutate(current_age = 2022 - birth_year) %>%
  filter(current_age < 100) %>%
  ggplot(aes(x = current_age, y = ..density..)) +
  geom_density(lwd = .75, fill = "lightgray") +
  guides(fill = "none", colour = "none") +
  labs(x = "Age",
       y = "Density") +
  theme_classic() +
  theme(plot.title = element_text(color = "black",
                                  size = 20,
                                  face = "bold")) +
  xlim(0, 100) +
  geom_vline(
    aes(xintercept = median(current_age)),
    color = "red",
    size = 0.75,
    linetype = "longdash"
  ) +
  geom_text(aes(
    median(current_age),
    y = 0.03,
    label = paste("median =", median(current_age)),
    hjust = -.3,
  ),
  size = 5.9,
  fontface = "plain")

p1 + p2

```


    
![png](output_75_0.png)
    






```R
options(repr.plot.width=12, repr.plot.height=4) #Set plot size 

is.outlier <- function(x) {
  return(x  < quantile(x, 0.25) - 1.5 * IQR(x) |
           x > quantile(x, 0.75) + 1.5 * IQR(x))
}

data %>%
  filter(!is.na(birth_year)) %>%
  mutate(current_age = 2022 - birth_year) %>%
  mutate(Outlier = ifelse(is.outlier(current_age), current_age, "")) %>%
  ggplot(aes(x = current_age, y = "")) +
  geom_boxplot(
    fill = "lightgray",
    outlier.shape = 5,
    outlier.color = "red",
    outlier.size = 4
  ) +
  labs(title = "Boxplot of Current Age",
       x = "Age",
       y = "") +
  theme_classic() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y  = element_blank()) +
  theme(plot.title = element_text(size = 14,
                                  face = "bold")) +
  geom_text_repel(aes(label = ifelse(duplicated(Outlier), "", Outlier))) +
  annotate("text", x = 41 + 5, y = 1.5, label = paste("median = 41"), fontface = 2)

```


    
![png](output_77_0.png)
    






```R
data %>%
  filter(!is.na(birth_year)) %>%
  mutate(current_age = 2022 - birth_year) %>%
  summarise(Mean =  round(mean(current_age),1),
            SD =  round(sd(current_age),1),
            Median = median(current_age),
            Min = min(current_age),
            Max = max(current_age),
            NAs = sum(is.na(age))
            ) %>%
  kable(caption = "TABLE 1.11: Summary of Current Ages", format = "html", table.attr = "style='width:40%;'") %>% 
  kable_classic() %>%
  kable_styling(bootstrap_options = c("striped"), font = 20) %>%
  row_spec(0,bold=TRUE) %>%
  as.character() %>%
  IRdisplay::display_html()

```


<table style='width:40%; font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto; font-size: 20px; margin-left: auto; margin-right: auto;' class=" lightable-classic table table-striped">
<caption style="font-size: initial !important;">TABLE 1.11: Summary of Current Ages</caption>
 <thead>
  <tr>
   <th style="text-align:right;font-weight: bold;"> Mean </th>
   <th style="text-align:right;font-weight: bold;"> SD </th>
   <th style="text-align:right;font-weight: bold;"> Median </th>
   <th style="text-align:right;font-weight: bold;"> Min </th>
   <th style="text-align:right;font-weight: bold;"> Max </th>
   <th style="text-align:right;font-weight: bold;"> NAs </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 45.6 </td>
   <td style="text-align:right;"> 17.6 </td>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 128 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
</tbody>
</table>


<br>

------------------------------------------------------------------------

### <span style="color: steelblue;">Coming Out Year</span>



```R
options(repr.plot.width=12, repr.plot.height=8) #Set plot size 

p1 <- data %>%
  filter(!is.na(coming_out_year), 
         coming_out_year > 1940) %>%
  ggplot(aes(x = coming_out_year)) +
  geom_histogram(
    fill = "#85D4E3",
    colour = 1,
    binwidth = 2,
    size = .25
  ) +
  guides(fill = "none", colour = "none") +
  labs(title = "Figure 1.12",
       x = "Year",
       y = "Count") +
  theme_classic() +
  theme(plot.title = element_text(color = "black",
                                  size = 20,
                                  face = "bold"))

p2 <- data %>%
  filter(!is.na(coming_out_year),
         coming_out_year > 1940) %>%
  ggplot(aes(x = coming_out_year, y = ..density..)) +
  geom_density(lwd = .75, fill = "lightgray") +
  guides(fill = "none", colour = "none") +
  labs(x = "Year",
       y = "Density") +
  theme_classic() +
  theme(plot.title = element_text(color = "black",
                                  size = 20,
                                  face = "bold")) +
  geom_vline(
    aes(xintercept = median(coming_out_year)),
    color = "red",
    size = 0.75,
    linetype = "longdash"
  ) +
  geom_text(aes(
    median(coming_out_year),
    y = 0.05,
    label = paste("median =", median(coming_out_year)),
    hjust = 1.2
  ),
  size = 5.9,
  fontface = "plain")

p1 + p2

```


    
![png](output_81_0.png)
    






```R
options(repr.plot.width=12, repr.plot.height=4) #Set plot size 

is.outlier <- function(x) {
  return(x  < quantile(x, 0.25) - 1.5 * IQR(x) |
           x > quantile(x, 0.75) + 1.5 * IQR(x))
}

data %>%
  filter(!is.na(coming_out_year)) %>%
  mutate(Outlier = ifelse(is.outlier(coming_out_year), coming_out_year, "")) %>%
  ggplot(aes(x = coming_out_year, y = "")) +
  geom_boxplot(
    fill = "lightgray",
    outlier.shape = 5,
    outlier.color = "red",
    outlier.size = 4
  ) +
  labs(title = "Boxplot of Coming Out Year",
       x = "Year",
       y = "") +
  theme_classic() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y  = element_blank()) +
  theme(plot.title = element_text(size = 14,
                                  face = "bold")) +
  geom_text_repel(aes(label = ifelse(duplicated(Outlier), "", Outlier))) +
  annotate("text", x = median(data$coming_out_year, na.rm = T) -2, y = 1.5, label = paste("median = ", median(data$coming_out_year, na.rm = T)), fontface = 2)

```


    
![png](output_83_0.png)
    






```R
data %>%
  filter(!is.na(coming_out_year)) %>%
  summarise(Mean =  round(mean(coming_out_year)),
            SD =  round(sd(coming_out_year)),
            Median = median(coming_out_year),
            Min = min(coming_out_year),
            Max = max(coming_out_year),
            NAs = sum(is.na(age))
            ) %>%
  kable(caption = "TABLE 1.12: Summary of Coming Out Years", format = "html", table.attr = "style='width:40%;'") %>% 
  kable_classic() %>%
  kable_styling(bootstrap_options = c("striped"), font = 20) %>%
  row_spec(0,bold=TRUE) %>%
  as.character() %>%
  IRdisplay::display_html()

```


<table style='width:40%; font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto; font-size: 20px; margin-left: auto; margin-right: auto;' class=" lightable-classic table table-striped">
<caption style="font-size: initial !important;">TABLE 1.12: Summary of Coming Out Years</caption>
 <thead>
  <tr>
   <th style="text-align:right;font-weight: bold;"> Mean </th>
   <th style="text-align:right;font-weight: bold;"> SD </th>
   <th style="text-align:right;font-weight: bold;"> Median </th>
   <th style="text-align:right;font-weight: bold;"> Min </th>
   <th style="text-align:right;font-weight: bold;"> Max </th>
   <th style="text-align:right;font-weight: bold;"> NAs </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 2008 </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:right;"> 1921 </td>
   <td style="text-align:right;"> 2021 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>


<br>

------------------------------------------------------------------------

### <span style="color: steelblue;">Age at Coming Out</span>



```R
options(repr.plot.width=12, repr.plot.height=8) #Set plot size 

p1 <- data %>% 
  filter(!is.na(age)) %>%
  ggplot(aes(x = age)) +
  geom_histogram(
    fill = "#85D4E3",
    colour = 1,
    binwidth = 2,
    size = .25
  ) +
  guides(fill = "none", colour = "none") +
  labs(title = "Figure 1.13",
       x = "Age",
       y = "Count") +
  theme_classic() +
  theme(plot.title = element_text(color = "black",
                                  size = 20,
                                  face = "bold"))

p2 <- data %>%
  filter(!is.na(age)) %>%
  ggplot(aes(x = age, y = ..density..)) +
  geom_density(lwd = .75, fill = "lightgray") +
  guides(fill = "none", colour = "none") +
  labs(x = "Age",
       y = "Density") +
  theme_classic() +
  theme(plot.title = element_text(color = "black",
                                  size = 20,
                                  face = "bold")) +
  geom_vline(
    aes(xintercept = median(age)),
    color = "red",
    size = 0.75,
    linetype = "longdash"
  ) +
  annotate("text",
           x = median(data$age, na.rm = T),
           y = 0.035,
           label = paste("median =", median(data$age, na.rm = T)),
           hjust = -.35,
           fontface = 2,
           size = 5.9)

p1 + p2

```


    
![png](output_87_0.png)
    






```R
options(repr.plot.width=12, repr.plot.height=4) #Set plot size 

is.outlier <- function(x) {
  return(x  < quantile(x, 0.25) - 1.5 * IQR(x) |
           x > quantile(x, 0.75) + 1.5 * IQR(x))
}

data %>%
  filter(!is.na(age)) %>%
  mutate(Outlier = ifelse(is.outlier(age), age, "")) %>%
  ggplot(aes(x = age, y = "")) +
  geom_boxplot(
    fill = "lightgray",
    outlier.shape = 5,
    outlier.color = "red",
    outlier.size = 4
  ) +
  labs(title = "Boxplot of Coming Out Age",
       x = "Age",
       y = "") +
  theme_classic() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y  = element_blank()) +
  theme(plot.title = element_text(size = 14,
                                  face = "bold")) +
  geom_text_repel(aes(label = ifelse(duplicated(Outlier), "", Outlier))) +
  annotate("text", x = median(data$age, na.rm = T) + 10, y = 1.5, label = paste("median = ", median(data$age, na.rm = T)), fontface = 2)

```


    
![png](output_89_0.png)
    






```R
data %>%
  filter(!is.na(age)) %>%
  summarise(Mean =  round(mean(age),1),
            SD =  round(sd(age),1),
            Median = median(age),
            Min = min(age),
            Max = max(age),
            NAs = sum(is.na(age))
            ) %>%
  kable(caption = "TABLE 1.13: Summary of Coming Out Age", format = "html", table.attr = "style='width:40%;'") %>% 
  kable_classic() %>%
  kable_styling(bootstrap_options = c("striped"), font = 20) %>%
  row_spec(0,bold=TRUE) %>%
  as.character() %>%
  IRdisplay::display_html()
```


<table style='width:40%; font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto; font-size: 20px; margin-left: auto; margin-right: auto;' class=" lightable-classic table table-striped">
<caption style="font-size: initial !important;">TABLE 1.13: Summary of Coming Out Age</caption>
 <thead>
  <tr>
   <th style="text-align:right;font-weight: bold;"> Mean </th>
   <th style="text-align:right;font-weight: bold;"> SD </th>
   <th style="text-align:right;font-weight: bold;"> Median </th>
   <th style="text-align:right;font-weight: bold;"> Min </th>
   <th style="text-align:right;font-weight: bold;"> Max </th>
   <th style="text-align:right;font-weight: bold;"> NAs </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 31.8 </td>
   <td style="text-align:right;"> 10.6 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 72 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>


<br>

------------------------------------------------------------------------

## <span style="color: lightgrey;">3.2</span> Multivariate Analysis

<br>

------------------------------------------------------------------------

### <span style="color: steelblue;">Coming Out Age & Birth Year</span>

#### <span style="color: gray;">Are younger generations coming out at a younger age?</span>



```R
options(repr.plot.width=10, repr.plot.height=6) #Set plot size 

data %>%
  mutate(decade = cut(birth_year, breaks = c(0, 1946, 1965, 1977, 1995, Inf),
    labels = c("Pre-Boomer", "Boomer", "Gen X", "Millenial", "Gen Z")), include.lowest = TRUE, exclude=NULL) %>%
  group_by(decade) %>%
  filter(!is.na(age)) %>%
  ggplot(aes(x = birth_year, y = age, color = decade)) +
  geom_point(aes(color = decade, alpha = 0.5)) +
  geom_smooth(formula = y ~ x, method=lm, se=FALSE) + 
  labs(title = "Figure 2.1a",
       subtitle = "Coming Out Age and Generation",
       x = "Year",
       y = "Coming Out Age") +
  guides(color=guide_legend(title="Generation"),
         alpha="none") +
  theme_classic() + 
  theme(plot.title = element_text(color = "black",
                                  size = 14,
                                  face = "bold")) +
  theme(axis.text.x = element_text(size=12)) +
  scale_color_manual(values =wes_palette("Moonrise3", 5))
```


    
![png](output_93_0.png)
    






```R
options(repr.plot.width=10, repr.plot.height=6) #Set plot size 

data %>%
  mutate(decade = cut(birth_year, breaks = c(0, 1946, 1965, 1977, 1995, Inf),
    labels = c("Pre-Boomer", "Boomer", "Gen X", "Millenial", "Gen Z")), include.lowest = TRUE, exclude=NULL) %>%
  group_by(decade) %>%
  filter(!is.na(age)) %>%
  ggplot(aes(x = decade, y = age, fill = decade)) +
  geom_boxplot() +
  labs(title = "Figure 2.1b",
       subtitle = "Coming Out Age and Generation",
       x = "",
       y = "Coming Out Age") +
  guides(fill=guide_legend(title="Generation"),
         alpha="none") +
  theme_classic() + 
  theme(plot.title = element_text(color = "black",
                                  size = 14,
                                  face = "bold")) +
  scale_fill_manual(values =wes_palette("Moonrise3", 5)) +
  theme(axis.text.x = element_text(size=12)) +
  geom_smooth(formula = y ~ x, method = "lm", se=TRUE, aes(group=1), color = "dimgray", linetype="dashed", size = .5)
```


    
![png](output_95_0.png)
    


<br>




```R
data %>% 
  mutate(decade = cut(birth_year, breaks = c(0, 1946, 1965, 1977, 1995, Inf),
    labels = c("Pre-Boomer", "Boomer", "Gen X", "Millenial", "Gen Z")), include.lowest = TRUE, exclude=NULL) %>%
  group_by(decade) %>%
  filter(!is.na(age)) %>%
  summarize(count = n(),
            mean = round(mean(age), 2)) %>%
  kable(caption = "TABLE 2.1: Birth Decade and Coming Out Age", format = "html", table.attr = "style='width:65%;'", col.names = c("Decade","Count","Mean Coming Out Age")) %>% 
  kable_classic() %>%
  kable_styling(bootstrap_options = c("striped"), font = 20) %>%
  row_spec(0,bold=TRUE) %>%
  as.character() %>%
  IRdisplay::display_html()

```


<table style='width:65%; font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto; font-size: 20px; margin-left: auto; margin-right: auto;' class=" lightable-classic table table-striped">
<caption style="font-size: initial !important;">TABLE 2.1: Birth Decade and Coming Out Age</caption>
 <thead>
  <tr>
   <th style="text-align:left;font-weight: bold;"> Decade </th>
   <th style="text-align:right;font-weight: bold;"> Count </th>
   <th style="text-align:right;font-weight: bold;"> Mean Coming Out Age </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Pre-Boomer </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 47.70 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Boomer </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 42.14 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Gen X </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 35.58 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Millenial </td>
   <td style="text-align:right;"> 89 </td>
   <td style="text-align:right;"> 27.52 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Gen Z </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 17.30 </td>
  </tr>
</tbody>
</table>


<br>

------------------------------------------------------------------------

### <span style="color: steelblue;">Coming Out Age & Race</span>

#### <span style="color: gray;">What does coming out age look across race?</span>



```R
options(repr.plot.width=10, repr.plot.height=7) #Set plot size 

data %>%
  group_by(race) %>%
  filter(race != "Unknown") %>%
  ggplot(aes(x = race, y = age, fill = race)) +
  geom_boxplot() +
  geom_jitter(shape = 16,
      color = "steelblue",
      position = position_jitter(0.21)) +
  theme_classic() +
  scale_fill_manual(values = wes_palette("Moonrise3", 5, type = "continuous")) +
  labs(title = "Figure 2.2",
       subtitle = "Coming Out Age by Race",
       x = "",
       y = "Coming Out Age") + 
  theme(axis.text.x = element_text(size=12)) +
  theme(plot.title = element_text(color = "black",
                                  size = 14,
                                  face = "bold"))

```


    
![png](output_99_0.png)
    


<br>




```R
data %>%
  filter(!is.na(age)) %>%
  group_by(race) %>%
  summarize( n(),
            round(mean(age),2),
            round(median(age),2),
            round(min(age),2),
            round(max(age),2)) %>%
  kable(caption = "TABLE 2.2: Race and Coming Out Age", format = "html", table.attr = "style='width:50%;'", col.names = c("Race","Count","Mean", "Median", "Min", "Max")) %>% 
  kable_classic() %>%
  kable_styling(bootstrap_options = c("striped"), font = 20) %>%
  row_spec(0,bold=TRUE) %>%
  as.character() %>%
  IRdisplay::display_html()

```


<table style='width:50%; font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto; font-size: 20px; margin-left: auto; margin-right: auto;' class=" lightable-classic table table-striped">
<caption style="font-size: initial !important;">TABLE 2.2: Race and Coming Out Age</caption>
 <thead>
  <tr>
   <th style="text-align:left;font-weight: bold;"> Race </th>
   <th style="text-align:right;font-weight: bold;"> Count </th>
   <th style="text-align:right;font-weight: bold;"> Mean </th>
   <th style="text-align:right;font-weight: bold;"> Median </th>
   <th style="text-align:right;font-weight: bold;"> Min </th>
   <th style="text-align:right;font-weight: bold;"> Max </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Asian </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 35.64 </td>
   <td style="text-align:right;"> 31.5 </td>
   <td style="text-align:right;"> 23 </td>
   <td style="text-align:right;"> 68 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Black </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 32.11 </td>
   <td style="text-align:right;"> 31.0 </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 55 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Latinx </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 28.90 </td>
   <td style="text-align:right;"> 27.0 </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 41 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Multiracial </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 24.67 </td>
   <td style="text-align:right;"> 25.0 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 33 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Unknown </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 41.00 </td>
   <td style="text-align:right;"> 41.0 </td>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:right;"> 41 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> White </td>
   <td style="text-align:right;"> 95 </td>
   <td style="text-align:right;"> 32.60 </td>
   <td style="text-align:right;"> 31.0 </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 72 </td>
  </tr>
</tbody>
</table>


<br>

------------------------------------------------------------------------

### <span style="color: steelblue;">Coming Out Age & Gender</span>

#### <span style="color: gray;">What does coming out age look across race?</span>



```R
options(repr.plot.width=10, repr.plot.height=7) #Set plot size 

data %>%
  group_by(gender) %>%
  filter(!is.na(gender)) %>%
  # Collapse the levels of Queer, Unspecified Queer, Genderqueer to just Queer
  mutate(gender = fct_collapse(gender, Queer = c("Queer","Genderqueer", "Unspecified queer"))) %>% 
  ggplot(aes(x = gender, y = age, fill = gender)) +
  geom_boxplot() +
  geom_jitter(shape = 16,
      color = "steelblue",
      position = position_jitter(0.21)) +
  theme_classic() +
  scale_fill_manual(values = wes_palette("Moonrise3", 7, type = "continuous")) +
  labs(title = "Figure 2.3",
       subtitle = "Coming Out Age by Gender",
       x = "",
       y = "Coming Out Age") + 
  theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1)) +
  theme(plot.title = element_text(color = "black",
                                  size = 14,
                                  face = "bold"))

```


    
![png](output_103_0.png)
    


<br>




```R
data %>%
  filter(!is.na(age),
         !is.na(gender)) %>%
  group_by(gender) %>%
  # Collapse the levels of Queer, Unspecified Queer, Genderqueer to just Queer
  mutate(gender = fct_collapse(gender, Queer = c("Queer","Genderqueer", "Unspecified queer"))) %>% 
  summarize( n(),
            round(mean(age),2),
            round(median(age),2),
            round(min(age),2),
            round(max(age),2)) %>%
  kable(caption = "TABLE 2.3: Gender and Coming Out Age", format = "html", table.attr = "style='width:60%;'", col.names = c("Gender","Count","Mean", "Median", "Min", "Max")) %>% 
  kable_classic() %>%
  kable_styling(bootstrap_options = c("striped"), font = 20) %>%
  row_spec(0,bold=TRUE) %>%
  as.character() %>%
  IRdisplay::display_html()
```


<table style='width:60%; font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto; font-size: 20px; margin-left: auto; margin-right: auto;' class=" lightable-classic table table-striped">
<caption style="font-size: initial !important;">TABLE 2.3: Gender and Coming Out Age</caption>
 <thead>
  <tr>
   <th style="text-align:left;font-weight: bold;"> Gender </th>
   <th style="text-align:right;font-weight: bold;"> Count </th>
   <th style="text-align:right;font-weight: bold;"> Mean </th>
   <th style="text-align:right;font-weight: bold;"> Median </th>
   <th style="text-align:right;font-weight: bold;"> Min </th>
   <th style="text-align:right;font-weight: bold;"> Max </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Cis Man </td>
   <td style="text-align:right;"> 74 </td>
   <td style="text-align:right;"> 33.16 </td>
   <td style="text-align:right;"> 31.5 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 68 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cis Woman </td>
   <td style="text-align:right;"> 62 </td>
   <td style="text-align:right;"> 32.05 </td>
   <td style="text-align:right;"> 30.0 </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 72 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Queer </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 19.75 </td>
   <td style="text-align:right;"> 20.5 </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 24 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Non binary </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 28.00 </td>
   <td style="text-align:right;"> 22.0 </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 49 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Trans Man </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 26.33 </td>
   <td style="text-align:right;"> 26.0 </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 34 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Trans Woman </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 31.54 </td>
   <td style="text-align:right;"> 28.0 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 64 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Unspecified </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 29.00 </td>
   <td style="text-align:right;"> 27.5 </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 41 </td>
  </tr>
</tbody>
</table>


<br>

------------------------------------------------------------------------

### <span style="color: steelblue;">Coming Out Age & Sexuality</span>

#### <span style="color: gray;">What does coming out age look across sexuality?</span>



```R
options(repr.plot.width=10, repr.plot.height=7) #Set plot size 

data %>%
  group_by(sexuality) %>%
  filter(!is.na(sexuality),
         age < 100) %>%
  # Recode "Unspecified non heterosexual" to shorten
  mutate(sexuality = fct_recode(sexuality, "Non heterosexual" = "Unspecified non heterosexual")) %>%
  mutate(sexuality = fct_collapse(sexuality, Other = c("Pansexual","Sexually fluid"))) %>% 
  ggplot(aes(x = sexuality, y = age, fill = sexuality)) +
  geom_boxplot() +
  geom_jitter(shape = 16,
      color = "steelblue",
      position = position_jitter(0.21)) +
  theme_classic() +
  scale_fill_manual(values = wes_palette("Moonrise3", 8, type = "continuous")) +
  labs(title = "Figure 2.4",
       subtitle = "Coming Out Age by Sexuality",
       x = "",
       y = "Coming Out Age") + 
  theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1)) +
  theme(plot.title = element_text(color = "black",
                                  size = 14,
                                  face = "bold"))
```


    
![png](output_107_0.png)
    






```R
data %>%
  filter(!is.na(age),
         !is.na(sexuality)) %>%
  group_by(sexuality) %>%
  # Recode "Unspecified non heterosexual" to shorten
  mutate(sexuality = fct_recode(sexuality, "Non heterosexual" = "Unspecified non heterosexual")) %>%
  summarize( n(),
            round(mean(age),2),
            round(median(age),2),
            round(min(age),2),
            round(max(age),2)) %>%
  kable(caption = "TABLE 2.4: Sexuality and Coming Out Age", format = "html", table.attr = "style='width:60%;'", col.names = c("Sexuality","Count","Mean", "Median", "Min", "Max")) %>% 
  kable_classic() %>%
  kable_styling(bootstrap_options = c("striped"), font = 20) %>%
  row_spec(0,bold=TRUE) %>%
  as.character() %>%
  IRdisplay::display_html()

```


<table style='width:60%; font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto; font-size: 20px; margin-left: auto; margin-right: auto;' class=" lightable-classic table table-striped">
<caption style="font-size: initial !important;">TABLE 2.4: Sexuality and Coming Out Age</caption>
 <thead>
  <tr>
   <th style="text-align:left;font-weight: bold;"> Sexuality </th>
   <th style="text-align:right;font-weight: bold;"> Count </th>
   <th style="text-align:right;font-weight: bold;"> Mean </th>
   <th style="text-align:right;font-weight: bold;"> Median </th>
   <th style="text-align:right;font-weight: bold;"> Min </th>
   <th style="text-align:right;font-weight: bold;"> Max </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Bisexual </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 28.21 </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 43 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Gay </td>
   <td style="text-align:right;"> 64 </td>
   <td style="text-align:right;"> 33.84 </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 68 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Heterosexual </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 42.80 </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 65 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Lesbian </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 33.48 </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 50 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pansexual </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:right;"> 32 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Queer </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 28.23 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 43 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sexually fluid </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 22.00 </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 22 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Unspecified </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 29.53 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 50 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Non heterosexual </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 30.85 </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 72 </td>
  </tr>
</tbody>
</table>


<br>
