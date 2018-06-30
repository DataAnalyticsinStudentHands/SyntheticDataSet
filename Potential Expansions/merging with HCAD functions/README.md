# Harris County Appraisal District functions
This folder contains scripts for cleaning and subsetting the Harris County Appraisal District shapefiles, and placing simulated households from the citymodels dataset into geographical houses.

The script prepare_HCAD_parcels.R should be sourced first with the shapefiles and accompanying txt files from HCAD available here: http://pdata.hcad.org/download/index.html. Text files of interest include Building_res.txt, Building_res.txt, and Structural_elem1.txt. The shapefiles for tracts should also be included in the working directory. The script will filter and write 2 RDS files with valid geographical parcels of structures that people should be able to live in for both households and group quarters.

The script mergeHCADparcelsparallel.R should be sourced with the model and outputs of the script prepare_HCAD_parcels.R to place simulated households into physical structures. It should be modified for the resources the user has available. The merge matches households and group quarters populations into geographical locations of sructures based on the harris county appraisal district building style code. A list of the codes is available here: http://hcad.org/hcad-resources/hcad-appraisal-codes/hcad-building-style-codes/ .

Single households were placed in the following building style codes
+ 101- Residential 1 Family
+ 107- Townhome with Common Element
+ 108- Single Wide Residential Mobile Home
+ 109- Double Wide Residential Mobile Home
+ 110- Unsound Residential Structure
+ 125- Farm with Dwelling
+ 8177- Townhouse, High-rise- End Unit
+ 8178- Townhouse, High-rise- Inside Unit
+ 8179- Townhouse, High-rise- Detached
+ 8351- Single-Family Residence
+ 8354- Townhouse, Inside Unit
+ 8401- Townhouse, End Unit
+ 8548-	Urban Row House, Detached
+ 8549-	Urban Row House, End Unit
+ 8550-	Urban Row House, Inside Unit
+ 8986- Int. Space, Townhouse, Inside Unit
+ 8988- Int. Space, Townhouse, End Unit
+ 8774	Shell, Townhouse, End Unit
+ 8775	Shell, Townhouse, Inside Unit

Two households were placed in the following building style code
+ 102- Residential 2 Family

Three households were placed in the following building style code
+ 103- Residential 3 Family

Four households were placed in the following building style code
+ 104- Residential 4 Family

Group quarters populations were randomly placed in buildings with the following building style codes
+ 620 Church
+ 640 Hospital
+ 660- Fire Station (Staff)
+ 685 Governmental Building
+ 8321- Dormitory
+ 8324- Fraternity House
+ 8393- Labor Dormitory
+ 8424- Group Care Home
+ 8451- Multiple Res. (Sen. Citizen)
+ 8589- Elderly Assist. Multi. Res.
+ 8313- Convalescent Hospital
+ 8322- Fire Station (Staff)
+ 8330- Home For The Elderly
+ 8335- Jail- Correctional Facility
+ 8348- Rectory
+ 8394- Transcient Labor Cabin
+ 8156- Alternative School
+ 8551- Rooming House
+ 8588- Extended Stay Motel
+ 8710- Retirement Community Complex
+ 8331- Hospital
+ 8343- Motel
+ 8309- Church
+ 8489- Jail- Police Station
+ 8311- Clubhouse
+ 8327- Governmental Building
+ 8491- Community Service Building
+ 8514- Community Center
+ 8308 Church w/ Sunday School

All remaining simulated households were placed randomly into buildings with the following building style codes, sometimes multiple households per building
+ 105- Mixed Res/Com, Res Structure
+ 8300- Apartment
+ 8352- Multiple Res (Low Rise)
+ 8338- Loft
+ 8459- Mixed Retail w/ Resid. Units
+ 8493- Flathouse
+ 8546- Senior Citizen Townhouse, End Unit
+ 8547- Senior Citizen Townhouse, Inside Unit
+ 8596- Shell, Apartment
+ 8984- Luxury Apartment
+ 8987- Int. Space, Multiple Resid.
+ 8989- Int. Space, Apartment
+ 8451 Multiple Res. (Sen. Citizen)
+ 8985 Senior Center

The following codes were not used
+ 106 Condominium (Common Element) (these are usually resources available to an entire living community, not usually a residence)
+ 126 Farm with Auxiliary Building
+ 610 Gymnasium (School)
+ 611 Library, Public
+ 612 Commons (Schools
+ 613 College (Entire) (Dorms are listed separately)
+ 614 Post Office, Main
+ 630 Auditorium
+ 680 Musuem
+ 690 Passenger Terminal
+ 8100 Bank Barn, General Purpose
+ 8101 Bank Barn, Special Purpose
+ 8102 Barn, General Purpose
+ 8103 Barn, Special Purpose
+ 8104 Commodity Warehouse
+ 8105 Confinement Barn
+ 8106 Controlled Atmosphere Storage
+ 8107 Corn Crib, Spaced Board
+ 8108 Corn Crib, Wire
+ 8109 Hog Breeding
+ 8110 Hog Farrowing
+ 8111 Hog Finishing
+ 8112 Hog Nursery
+ 8113 Loafing Shed
+ 8114 Milkhouse Shed
+ 8115 Poultry House, Cage Op, 1 Sty
+ 8116 Poultry House, Cage Op, 2 Sty
+ 8117 Poultry House, Floor Op, Breeder
+ 8118 Poultry House, Floor Op, Broiler
+ 8119 Turkey Barn
+ 8120 Sheep Shed
+ 8121 Tobacco Barn, Air Curing
+ 8122 Tobacco Barn Flue Curing
+ 8123	Seed Processing Storage
+ 8124	Lean-to, Farm Utility
+ 8125	Poultry House, Cage Op, Scr, 1 Sty El
+ 8126	Poultry House, Cage Op, Scr 2 Sty El
+ 8127	Winery Shop
+ 8131	Corn Crib, Bin
+ 8132	Individual Livestock Shelter
+ 8133	Prefabricated Storage Shed
+ 8134	Greenhouse, Modified Hoop, Small
+ 8135	Greenhouse, Hoop, Arch-Rib, Small
+ 8136	Greenhouse, Straight-Wall, Small
+ 8137	Greenhouse, Modified Hoop, Large
+ 8138	Greenhouse, Hoop, Arch-Rib, Large
+ 8139	Greenhouse, Straight-Wall, Large
+ 8140	Greenhouse, Modified Hoop, Medium
+ 8141	Greenhouse, Hoop, Arch-Rib, Medium
+ 8142	Greenhouse, Straight-Wall, Medium
+ 8150	Single Wide Commercial Mobile Home
+ 8151	Double Wide Commercial Mobile Home
+ 8152	Residential Garage - Detached
+ 8153	Residential Garage - Attached
+ 8154	Residential Garage - Built-in
+ 8155	Hunting Shelter
+ 8156	Alternative School
+ 8157	Maintenance Storage Building
+ 8158	Special Education Classroom
+ 8160	Machinery & Equipment
+ 8161	Personal Property
+ 8162	Outbuildings
+ 8163	Site Improvements
+ 8170	Institutional Greenhouse, Small
+ 8171	Institutional Greenhouse, Medium
+ 8172	Institutional Greenhouse, Large
+ 8173	Church Educational Wing
+ 8174	Pavillion
+ 8175	Skating Rink (Ice)
+ 8176	Skating Rinc (Roller)
+ 8180	Feed Handling and Mixing
+ 8181	Secure Storage Shed, Prefabricated
+ 8182	Calving Barn Shed
+ 8183	Golf Starter Booth
+ 8184	Arena Shelter
+ 8185	Truck Wash
+ 8300	Apartment
+ 8301	Armory
+ 8302	Auditorium
+ 8303	Automobile Showroom
+ 8304	Bank
+ 8305	Barn
+ 8306	Bowling Alley
+ 8310	City Club
+ 8311	Clubhouse
+ 8323 Fraternal Building (Houses are listed separately)
+ 8314	Country Club
+ 8315	Creamery
+ 8316	Dairy
+ 8317	Dairy Sales Building
+ 8318	Department Store
+ 8319	Discount Store
+ 8320	Dispensary
+ 8325	Service Garage (Obsolete)
+ 8326	Storage Garage
+ 8328	Storage Hangar
+ 8329	Hangar, Maint & Office
+ 8332	Hotel (Obsolete)
+ 8334	Industrial Mftg. (Obsolete)
+ 8336	Laundromat
+ 8337	Library, Public
+ 8339	Lumber Storage Shed, Horz.
+ 8340	Market
+ 8341	Medical Office
+ 8342	Mortuary
+ 8343	Motel
+ 8344	Office Building
+ 8345	Parking Structure
+ 8346	Post Office (Obsolete)
+ 8347	Poultry House (Obsolete)
+ 8348	Rectory
+ 8349	Fast Food Restaurant
+ 8350	Restaurant
+ 8353	Retail Store
+ 8354	Townhouse Inside Unit
+ 8355	Fine Arts & Crafts Building
+ 8356	Classroom
+ 8357	Commons (Schools)
+ 8358	Gymnasium (School)
+ 8359	Lecture Classroom (School)
+ 8360	Library, Media Center
+ 8361	Manual Arts Bldg (School)
+ 8362	Multi-Purpose Bldg (School)
+ 8363	Physical Education Building
+ 8364	Science Classroom
+ 8365	Elementary School (Entire)
+ 8366	Junior High School (Entire)
+ 8367	Arts & Crafts Bldg (Coll)
+ 8368	Classroom (College)
+ 8369	Commons (College)
+ 8370	Gymnasium (College)
+ 8371	Lecture Hall (College)
+ 8372	Library, College
+ 8373	Technical Trades Bldg.
+ 8374	Multi-Purp Bldg (College)
+ 8375	Phys Ed Bldg (College)
+ 8376	Science Building (College)
+ 8377	College (Entire)
+ 8378	Stable
+ 8379	Theater - Live Stage
+ 8380	Theater - Cinema
+ 8381	Veterinary Hospital
+ 8383	Tobacco Barn
+ 8384	Barber Shop
+ 8386	Mini-Warehouse
+ 8387	Transit Warehouse
+ 8388	Underground Prkg Structure
+ 8389	Equip. Storage Bldg. (Obs.)
+ 8390	Lumber Storage Bldg., Vert.
+ 8391	Material Storage Building
+ 8392	Industrial Engineering
+ 8395	Potato Storage
+ 8396	Hog Barn
+ 8397	Sheep Barn
+ 8398	Fruit Packing Barn
+ 8399	Cattle Shed (Obsolete)
+ 8400	Hay Shed (Obsolete)
+ 8403	Shower Building
+ 8404	Utility Building (Obsolete)
+ 8405	Skating Rink
+ 8406	Storage Warehouse
+ 8407	Distribution Warehouse
+ 8408	Service Station
+ 8409	T-Hangar
+ 8410	Automotive Center
+ 8412	Neighborhood Shopping Ctr
+ 8413	Community Shopping Center
+ 8414	Regional Shopping Center
+ 8416	Indoor Tennis Club
+ 8417	Handball-Racquetball Club
+ 8418	Health Club
+ 8419	Convenience Market
+ 8420	Bulk Fertilizer Storage
+ 8421	Grain Storage
+ 8422	Theater (Obsolete)
+ 8423	Mini-Lube Garage
+ 8426	Day Care Center
+ 8427	Fire Station (Volunteer)
+ 8428	Horse Arena
+ 8429	Hog Shed, Modified
+ 8430	Hog Shed
+ 8431	Outpatient Surgical Center
+ 8432	Restroom Building
+ 8434	Car Wash - Self Serve
+ 8435	Car Wash - Drive Thru
+ 8436	Car Wash - Automatic
+ 8440	Milkhouse
+ 8441	Cocktail Lounge
+ 8442	Bar/Tavern
+ 8443	Central Bank
+ 8444	Dental Office/Clinic
+ 8445	Log Home
+ 8446	Supermarket
+ 8447	Cold Storage Facility
+ 8448	Cold Storage, Farm
+ 8450	Cotton Gin
+ 8453	Industrial Flex Building
+ 8454	Shell, Industrial
+ 8455	Auto Dealership, Complete
+ 8456	Tool Shed
+ 8458	Discount Warehouse Store
+ 8460	Shell, Neigh. Shop. Ctr.
+ 8461	Shell, Community Shop. Ctr.
+ 8462	Shell, Regional Shop. Ctr.
+ 8465	Food Booth - Prefabricated
+ 8466	Boat Storage Shed
+ 8467	Boat Storage Building
+ 8468	Material Storage Shed
+ 8469	Freestall Barn
+ 8470	Equipment (Shop) Building
+ 8471	Lt. Commercial Utility Build.
+ 8472	Equipment Shed
+ 8473	Material Shelter
+ 8474	Poultry House, Cage Op., 1 Story
+ 8475	Poultry House, Floor Op. (Obsolete)
+ 8476	Farm Implement Building
+ 8477	Farm Utility Building
+ 8478	Farm Implement Shed
+ 8479	Farm Utility Storage Shed
+ 8480	Vegetable Storage
+ 8481	Museum
+ 8482	Convention Center
+ 8483	Fitness Center
+ 8484	High School (Entire)
+ 8485	Natatorium
+ 8486	Fieldhouse
+ 8487	Vocational School
+ 8488	Bookstore (School)
+ 8489	Jail - Police Station
+ 8490	Kennels
+ 8492	Shell, Office
+ 8494	Industrials, Light Mftg.
+ 8495	Industrials, Heavy Mftg.
+ 8496	Laboratory
+ 8497	Computer Center
+ 8498	Broadcasting Facility
+ 8499	Dry Cleaners-Laundry
+ 8508	Car Wash - Canopy
+ 8511	Drug Store
+ 8512	Luxury Boutique
+ 8513	Regional Discount Shopping Center
+ 8515	Casino
+ 8516	Fellowship Hall
+ 8517	Narthex/Foyer
+ 8518	Greenhouse, Lath Shade House
+ 8519	Greenhouse, Shade Shelter
+ 8520	Greenhouse, Modified Hoop (Obsolete)
+ 8521	Greenhouse, Hoop, Arch-Rib (Obsolete)
+ 8522	Greenhouse, Straight-Wall (Obsolete)
+ 8523	Golf Cart Storage Building
+ 8524	Corn Crib
+ 8525	Mini Warehouse, High-Rise
+ 8526	Service Garage Shed
+ 8527	Municipal Service Garage
+ 8528	Service Repair Garage
+ 8529	Snack Bar
+ 8530	Cafeteria
+ 8531	Mini-Mart Convenience Store
+ 8532	Florist Shop
+ 8533	Warehouse Food Store
+ 8534	Warehouse Showroom Store
+ 8537	Lodge
+ 8538	Guest Cottage
+ 8539	Bed and Breakfast Inn
+ 8540	Motel Room, 2 Sty., Dbl. Row
+ 8541	Motel Room, 2 Sty., Sgl. Row
+ 8542	Motel Room, 1 Sty., Dbl. Row
+ 8543	Motel Room, 1 Sty., Sgl. Row
+ 8544	Motel, Office-Apartment
+ 8552	Recreational Enclosure
+ 8554	Shed Office Structure
+ 8555	Lt. Commercial Arch-rib, Quon
+ 8556	Bulk Oil Storage
+ 8557	Farm Utility Arch-rib, Quon.
+ 8558	Farm Implement Arch-rib, Quon
+ 8559	Stable, High-value
+ 8560	Equestrian Lean-To
+ 8561	Feeder Barn
+ 8562	Farm Commodity Storage Shed
+ 8563	Bag Fertilizer Storage
+ 8564	Dehydrator Building
+ 8565	Farm Utility Shelter
+ 8566	Farm Sun Shade Shelter
+ 8567	Poultry House - Cage, 2 Sty.
+ 8568	Poultry House-Cage, 2 Sty.,El
+ 8569	Poultry House - Cage, 3 Sty.
+ 8570	Poultry House-Cage, 1 Sty, El
+ 8571	Passenger Terminal
+ 8573	Arcade Building
+ 8574	Visitor Center
+ 8575	Dining Atrium
+ 8576	Atrium
+ 8577	Parking Level
+ 8578	Mini-Bank
+ 8580	Truck Stop
+ 8581	Post Office, Main
+ 8582	Post Office, Branch
+ 8583	Mail Processing Facility
+ 8584	Mega Warehouse
+ 8585	Mechanical Penthouse
+ 8586	Roadside Market
+ 8587	Shell, Multiple Residence
+ 8588	Extended Stay Motel
+ 8594	Hotel, Full Service
+ 8595	Hotel, Limited Service
+ 8596	Shell, Apartment
+ 8597	Mixed Retail w/ Office Units
+ 8598	Relocatable Classroom
+ 8599	Relocatable Office
+ 8600	Administrative Office
+ 8700	Mall Anchor Department Store
+ 8710	Retirement Community Complex
+ 8718	Banquet Hall
+ 8719	Modular Restaurant
+ 8720	Mega Warehouse Discount Store
+ 8990	Int. Space, Neigh. Shop Ctr
+ 8991	Int. Space, Commun. Shop Ctr
+ 8992	Int. Space, Region. Shop Ctr
+ 8993	Interior Space, Office
+ 8994	Int. Space, Industrial
+ 9100	TEST - Manufacturing, Light
+ 9122	TEST - Office, Low-Rise, Interior Space
+ 9200	TEST - Warehouse, Light
+ 9215	TEST - Warehouse, Mega
+ 9220	TEST - Warehouse, Cold Storage

It writes a complete file for the entire sample set, as well as individual tract files with account numbers. If there were simulated group quarters populations and no group quarters building style codes in a tract, a separate file for the group quarters IDs in that tract will be written. If there were too many households for the number of residencies available and no buildings in the category for more than 4 households, then another file will be written for the household IDs in that tract.

# Bandaids

If mergeHCADparcelsparallel.R fails to complete due to lack of computational resources, the user can try running bandaid.R with the previously written tract files and inputs for mergeHCADparcelsparallel.R in the same folder. bandaid.R will join the previously written tracts together, run for the remaining tracts and then join the 2 sets together.

If the user wants to rerun for tracts with not enough buildings, they can use morecomplicatedbandaid.R which will join previously written files without secondary files of IDs that failed to merge, then rerun the remaining tracts and join the two sets.
