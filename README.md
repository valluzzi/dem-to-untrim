# DEM to UnTRIM Grid Converter

Programma Fortran90 per convertire un Digital Elevation Model (DEM) in un file griglia ottimizzato per il modello **UnTRIM** (Unstructured Triangular and Rectangular Mesh).

## Panoramica

Il programma `DEM_to_UnTRIM.f90` legge un file DEM in formato ASCII e genera:
- **UnTRIM.grd**: File griglia principale con geometria e topografia
- **link.grd**: File con informazioni sui collegamenti tra celle (optional)

## Struttura del file UnTRIM.grd

Il file UnTRIM.grd è un file **testuale formattato** che contiene la discretizzazione della griglia computazionale in UnTRIM, partendo dai dati del DEM.

### Intestazione (Namelist)

La prima sezione contiene un namelist Fortran `GRD_2008` con i parametri della griglia:

```
&GRD_2008
 NV      = 6062          ! Numero di vertici (nodi)
 NE      = 8432          ! Numero di poligoni/celle
 NR      = 5013          ! Parametro relativo alle regioni
 NS      = 20536         ! Numero totale di lati/edge
 NSI     = 14236         ! Lati interni
 NBC     = 123           ! Numero di condizioni al contorno
 NSF     = 14236         ! Lati di confine (frontera)
 TNE     = 1262892       ! Total number of sub-polygons (celle raffinate)
 TNS     = 463464        ! Total number of sub-edges (lati raffinati)
 ANGLE   = 0.0           ! Angolo di rotazione della griglia
 LOCATION = 'Cesenatico' ! Descrizione della zona
 /
```

**Parametri chiave:**
- **NV**: Numero totale di vertici nella griglia computazionale
- **NE**: Numero totale di poligoni (quadrilateri) nella griglia
- **NS**: Numero totale di lati della griglia
- **TNE/TNS**: Numeri totali delle sotto-celle/sotto-lati raffinati dal DEM (sub-grid)

### Dati della griglia

Dopo il namelist, il file contiene quattro sezioni:

#### 1. **Vertici della griglia computazionale** (NV righe)

```
         1    1570.69897461       6086.45849609
         2    1570.69897461       6184.62744141
         3    1472.53027344       6184.62744141
        ...
```

Formato: `ID_VERTICE  X_COORDINATA  Y_COORDINATA`

- **ID_VERTICE**: Numero sequenziale (1 a NV)
- **X_COORDINATA**: Coordinata X in double precision
- **Y_COORDINATA**: Coordinata Y in double precision

#### 2. **Poligoni (Celle) della griglia** (NE righe)

```
         1            4    1521.59387207       6135.54345703         1         2         3         4        -1        -1         1         1
         2            4    1521.59387207       6037.37475586         5         6         3         2        -1        -1         1         1
        ...
```

Formato: `ID  NVERT  XC  YC  V1  V2  V3  V4  IS1  IS2  IS3  IS4`

- **ID**: Identificativo cella (1 a NE)
- **NVERT**: Numero di vertici (sempre 4 per quadrilateri)
- **XC, YC**: Coordinate del centroide della cella
- **V1, V2, V3, V4**: Indici dei 4 vertici della cella
- **IS1-IS4**: Indici delle celle adiacenti (-1 se al confine)

#### 3. **Lati della griglia** (NS righe)

```
         1           1          2         -1        -1
         2           2          3         -1        -1
        ...
```

Formato: `ID  VERTICE_INIZIO  VERTICE_FINE  CELLA_SX  CELLA_DX`

- **ID**: Identificativo lato
- **VERTICE_INIZIO/FINE**: Indici dei due vertici che formano il lato
- **CELLA_SX/DX**: Indici delle celle adiacenti (-1 se esterno/confine)

#### 4. **Dati di raffinamento per celle** (TNE righe complessive)

Per ogni cella nella griglia originale (NE celle):

```
         1         123
  1.234567E+03  1.234567E+03  1.234567E+03  ...
  5.678901E+00  5.678901E+00  5.678901E+00  ...
```

Formato per cella `i`:
- Riga 1: `ID_CELLA  NUM_SUB_CELLE` (numero di sotto-celle della cella i)
- Riga 2: `AREA_1  AREA_2  ...` (aree delle sotto-celle, 10 valori per riga in G20.12)
- Riga 3: `QUOTA_1  QUOTA_2  ...` (quote topografiche delle sotto-celle, G20.12)

**Parametri:**
- **NUM_SUB_CELLE**: Numero di celle raffinate dal DEM per questa cella (determina raffinamento locale)
- **AREA**: Area della sotto-cella in m² (da DEM con fattore di scala)
- **QUOTA**: Elevazione della sotto-cella in m

#### 5. **Dati di raffinamento per lati** (TNS righe complessive)

Per ogni lato nella griglia originale (NSF lati di confine):

```
         1          45
  1.234567E+02  1.234567E+02  1.234567E+02  ...
  -5.67890E+00 -5.67890E+00 -5.67890E+00  ...
```

Formato per lato `j`:
- Riga 1: `ID_LATO  NUM_SUB_LATI` (numero di sotto-lati del lato j)
- Riga 2: `LUNGHEZZA_1  LUNGHEZZA_2  ...` (lunghezze dei sotto-lati, G20.12)
- Riga 3: `QUOTA_1  QUOTA_2  ...` (quote topografiche dei sotto-lati, G20.12)

**Parametri:**
- **NUM_SUB_LATI**: Numero di sub-lati raffinati per questo lato dal DEM
- **LUNGHEZZA**: Lunghezza del sub-lato
- **QUOTA**: Elevazione media della topografia lungo il sub-lato

## Processo di conversione

### Input: DEM ASCII

Il file DEM in input è un file ASCII (es. `lidar_V7.asc`) nel formato ESRI standard:

```
ncols         imx
nrows         jmx
xllcorner     xmin
yllcorner     ymin
cellsize      delta_x
NODATA_value  -290000
altura_1 altura_2 ... altura_imx
...
```

### Elaborazione principale

1. **Lettura DEM**: Carica la matrice di elevazione dal file ASCII
2. **Coarsening**: Raggruppa i pixel del DEM in celle della griglia computazionale
3. **Cloning**: Estrae celle omogenee (con stessa topografia) per ottimizzare lo storage
4. **Raffinamento locale**: Mantiene sub-grid di celle DEM per accurate quota
5. **Generazione griglia**: Crea vertici, lati e celle UnTRIM
6. **Output**: Scrive UnTRIM.grd in formato formattato

### Parametri di configurazione

Nel modulo `tinclude2`, i principali flag di controllo sono:

```fortran
LOGICAL :: clone=.true.          ! Estrae celle identiche (compression)
LOGICAL :: sparta=.false.        ! Gestisce spartiacque
LOGICAL :: RedBlack=.true.       ! Sorting ottimizzato
LOGICAL :: slim=.true.           ! Riduci memoria
LOGICAL :: island=.false.        ! Tratta isole separate
LOGICAL :: flat=.false.          ! Terreno piatto
LOGICAL :: argina=.true.         ! Arginamenti

INTEGER :: nl=100                ! Fattore di raffinamento (pixel per cella)
INTEGER :: region=4              ! 1=Campania, 2=Puglia, 3=Multi, 4=Cesenatico
```

## Formato numerico

- **Double Precision (G20.12)**: Coordinate e quote utilizzano 8 byte
- **Integer**: Indici e contatori
- **Scale factor**: 0.01 (le quote nel DEM vengono divise per 100)

Esempio:
- DEM valore: 56789 → Quota scalata: 567.89 m
- NODATA value: -290000 (terreno non disponibile)

## Dimensione file

Per il file di esempio `UnTRIM.grd`:
- **Dimensione**: ~76 MB
- **Vertici**: 6062
- **Celle computazionali**: 8432
- **Lati**: 20536
- **Sub-celle raffinate**: 1,262,892
- **Sub-lati raffinati**: 463,464

La grande dimensione è dovuta al mantenimento del sub-grid DEM per accuratezza topografica.

## Utilizzo

```bash
# Compilare
gfortran -O2 src/DEM_to_UnTRIM.f90 -o DEM_to_UnTRIM.exe

# Eseguire
./DEM_to_UnTRIM.exe
```

Il programma legge il file DEM dalla variabile stringa `DEM` nel codice e genera `UnTRIM.grd`.

## Output aggiuntivo

### link.grd

File testuale con informazioni sui collegamenti interni della griglia:

```
nlk tnl ... (intestazione)
ID_LINK  CELLA1  CELLA2  NUM_SUB_LINK
LUNGHEZZA_1  LUNGHEZZA_2  ...
QUOTA_1      QUOTA_2      ...
CAP_1        CAP_2        ...
```

Contiene i parametri di collegamento tra celle adiacenti della griglia UnTRIM.
