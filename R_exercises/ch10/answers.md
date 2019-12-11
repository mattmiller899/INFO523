
1.	Run Kmean or PAM on your dataset (run it multiple times without setting set.seed()). Report differences in performance between runs (Silhouette width or overlaps of observations in different clustering). Can you make sense of the clusters produced?

Subsetting the dataset down to the year 2008, we rand PAM to cluser all countries based on the 8 factors. We additionally applied a log transformation on the data. Running PAM multiple times Silhouette width is highest at 2 clusters and decreases with subsequent additions. Plotting a TSNE of PAM with 3 clusters and doing H-clust, a grouping of three cluster appears to work better than two.

Doing so the PAM method produced three clusters the results of which fit fairly well with our understanding of what we'd think of as 1st world (developed) 2nd world and 3rd world (developing) countries. The results seem to be a combination of two models. The first of which being the designation of 1st 2nd, and 3rd world countries (a relic of the cold war in the 20th century) and the more modern model, The Organisation for Economic Co-operation and Development OECD countries (designed as developed countries). 

```
                   country pam_fit.cluster
1                  Albania               1
2                  Armenia               1
3                Australia               1
4                  Austria               1
5               Azerbaijan               1
6                  Belarus               1
7                  Belgium               1
8   Bosnia and Herzegovina               1
9                   Brunei               1
10                Bulgaria               1
11                  Canada               1
12                   Chile               1
13                 Croatia               1
14                    Cuba               1
15                  Cyprus               1
16                 Denmark               1
17                 Estonia               1
18                 Finland               1
19                  France               1
20                 Germany               1
21                  Greece               1
22                 Hungary               1
23                 Iceland               1
24                 Ireland               1
25                  Israel               1
26                   Italy               1
27                   Japan               1
28              Kazakhstan               1
29                  Kuwait               1
30                  Latvia               1
31                 Lebanon               1
32               Lithuania               1
33              Luxembourg               1
34                   Malta               1
35                 Moldova               1
36             Netherlands               1
37             New Zealand               1
38                  Norway               1
39                  Poland               1
40                Portugal               1
41                   Qatar               1
42                 Romania               1
43                  Russia               1
44                  Serbia               1
45               Singapore               1
46         Slovak Republic               1
47                Slovenia               1
48                   Spain               1
49                  Sweden               1
50             Switzerland               1
51     Trinidad and Tobago               1
52                 Ukraine               1
53    United Arab Emirates               1
54          United Kingdom               1
55           United States               1
56                 Algeria               2
57               Argentina               2
58                 Bahrain               2
59              Bangladesh               2
60                 Bolivia               2
61                Botswana               2
62                  Brazil               2
63                Cambodia               2
64                   China               2
65                Colombia               2
66              Costa Rica               2
67                 Ecuador               2
68                   Egypt               2
69             El Salvador               2
70                 Georgia               2
71               Guatemala               2
72                Honduras               2
73                   India               2
74               Indonesia               2
75                    Iran               2
76                 Jamaica               2
77                  Jordan               2
78                   Libya               2
79                Malaysia               2
80                  Mexico               2
81                Mongolia               2
82                 Morocco               2
83                   Nepal               2
84               Nicaragua               2
85                    Oman               2
86                  Panama               2
87                Paraguay               2
88                    Peru               2
89             Philippines               2
90            Saudi Arabia               2
91            South Africa               2
92               Sri Lanka               2
93                   Syria               2
94              Tajikistan               2
95                Thailand               2
96                 Tunisia               2
97                  Turkey               2
98            Turkmenistan               2
99                 Uruguay               2
100             Uzbekistan               2
101              Venezuela               2
102                Vietnam               2
103                 Angola               3
104                  Benin               3
105               Cameroon               3
106       Congo, Dem. Rep.               3
107            Congo, Rep.               3
108          Cote d'Ivoire               3
109                Eritrea               3
110               Ethiopia               3
111                  Gabon               3
112                  Ghana               3
113                  Haiti               3
114                  Kenya               3
115             Mozambique               3
116                Myanmar               3
117                Nigeria               3
118               Pakistan               3
119                Senegal               3
120                  Sudan               3
121               Tanzania               3
122                   Togo               3
123                 Zambia               3
124               Zimbabwe               3
```

2.	Run a hierarchical method on your dataset, try a couple of different cuts. Are these results more meaningful than those from the participation method?

When using the ward.D2 method to do hierarchical clustering, with cut sizes of 2, 3 and 4, we find that the results of cutting the dendrogram into 3 partitions very closely matches the PAM clusters.  

```
with three clusters: to verify that the cluster labels 1, 2, 3 match PAM's mediods results
     1  2  3
  1 47  8  0
  2  2 45  0
  3  0  0 22
```
