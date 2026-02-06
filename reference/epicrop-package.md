# epicrop: SEIR Simulation Modelling of Crop Diseases

Generic simulation modelling of crop diseases using a
Susceptible-Exposed-Infectious-Removed ('SEIR') model. This type of
model was first described in botanical epidemiology by Zadoks (1971)
[doi:10.1094/Phyto-61-600](https://doi.org/10.1094/Phyto-61-600) and
implemented in the 'EPIRICE' model by Savary et al. (2012)
[doi:10.1016/j.cropro.2011.11.009](https://doi.org/10.1016/j.cropro.2011.11.009)
detailing the model's development, use and results from modelling global
unmanaged epidemics of rice diseases for five major rice diseases,
bacterial blight, brown spot, leaf blast, sheath blight and tungro.
Specific functions are provided to simulate all of these diseases with a
generic 'SEIR' model function that is suitable for parameterising for
use with other pathosystems as demonstrated in Savary et al. 2012 and
Kim et al. 2015
[doi:10.1016/j.agrformet.2015.01.011](https://doi.org/10.1016/j.agrformet.2015.01.011)
along with two wheat diseases, brown (leaf) rust and Septoria tritici
blotch as published in Savary et al. 2015
[doi:10.1007/s10658-015-0650-7](https://doi.org/10.1007/s10658-015-0650-7)
. Functions for fetching and formatting weather data from the NASA POWER
database are also provided to simplify the process of running
simulations.

## See also

Useful links:

- <https://codeberg.org/adamhsparks/epicrop>

- <https://adamhsparks.codeberg.page/epicrop>

- Report bugs at <https://codeberg.org/adamhsparks/epicrop/issues>

## Author

**Maintainer**: Adam H. Sparks <adamhsparks@gmail.com>
([ORCID](https://orcid.org/0000-0002-0061-8359))

Authors:

- Robert Hijmans <robert.hijmans@gmail.com>
  ([ORCID](https://orcid.org/0000-0001-5872-2872)) (Original R
  implementation) \[translator, copyright holder\]

- Serge Savary <serge.savary@toulouse.inra.fr>
  ([ORCID](https://orcid.org/0000-0002-6867-180X)) (Model conception and
  development in Stella) \[conceptor, copyright holder\]

- Ireno Pangga <ibpangga@yahoo.com> (Model conception, development in
  Stella and original R implementation) \[translator, copyright holder\]

- Jorrel Aunario <j.aunario@irri.org> (Original R implementation)
  \[translator, copyright holder\]

- Kwang-Hyung Kim <sospicy77@gmail.com> (Contributed code for modified
  Kim leaf blast and sheath blight functions and feedback on the
  implementation.) \[copyright holder\]

Other contributors:

- Aji Sukarta <aji.ixb@gmail.com> (Helped fix bug related to SEIR model
  and establishment date) \[contributor\]

- Emerson Del Ponte <delponte@ufv.br>
  ([ORCID](https://orcid.org/0000-0003-4398-409X)) (Suggested fixes for
  several typos in the documentation) \[contributor\]
