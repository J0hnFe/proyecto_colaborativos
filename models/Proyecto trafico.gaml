/**
* Name: Traffic
* Author: Patrick Taillandier & Alexis Drogoul
* Description: A simple traffic model with a pollution model: the speed on a road depends on the number of people 
* on the road (the highest, the slowest), and the people diffuse pollution on the envrionment when moving.
* Tags: gis, shapefile, graph, skill, transport, field
*/
model traffic

global {
	file building_shapefile <- file("../includes/buildings_trebol2.shp");
	file road_shapefile <- file("../includes/roads_trebol2.shp");
	geometry shape <- envelope(building_shapefile) + envelope(road_shapefile);
	float step <- 10 #s;
	field cell <- field(300, 300);
	graph road_network;
	map<road, float> road_weights;
	int carros <- 150 min: 0 max: 800;
	int buses <- 10 min: 0 max: 800;
	int motos <- 100 min: 0 max: 800;
	float avg_pollution <- 0.0;

	/////////////////////////////////////////////////////////////////////////////////////////
	// Bloque de inicialización del modelo. Se ejecuta una sola vez al inicio de la simulación
	init {
		create building from: building_shapefile;
		create road from: road_shapefile;
		create people number: carros {
			location <- any_location_in(one_of(building));
			state <- flip(0.75) ? "ok" : "notok";
		}

		// Creacion de transportes en ubicaciones iniciales
		create transporte_publico number: buses {
			location <- any_location_in(one_of(road));
		}

		create moto number: motos {
			location <- any_location_in(one_of(road)); // Se crean en cualquier punto de la carretera
		}

		road_weights <- road as_map (each::each.shape.perimeter);
		road_network <- as_edge_graph(road);
	}
	////////////////////////////////////////////////////////////////////////////////////////

	////////////////////////////////////////////////////////////////
	// Metodos reflex se ejecutan en cada paso de la simulacion
	reflex update_road_speed {
		road_weights <- road as_map (each::each.shape.perimeter / each.speed_coeff);
		road_network <- road_network with_weights road_weights;
	}

	reflex pollution_evolution {
		cell <- cell * 0.8;
		diffuse var: pollution on: cell proportion: 0.9;
	}

	reflex compute_avg_pollution {
		avg_pollution <- mean(cell);
		//write "Promedio de contaminacion: " + avg_pollution;
		avg_pollution <- floor(avg_pollution * 10000) / 10000; // Truncar a 4 decimales
	}
	


	////////////////////


	///////////////////////////////////////////////////////////////
}

//////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////// Species (clases) del experimento//////////////////////////////////////
species people skills: [moving] {
	point target;
	float leaving_proba <- 0.05;
	float speed <- rnd(10) #km / #h + 1;
	string state;

	reflex leave when: (target = nil) and (flip(leaving_proba)) {
		target <- any_location_in(one_of(building));
	}

	reflex move when: target != nil {
		path path_followed <- goto(target: target, on: road_network, recompute_path: false, return_path: true, move_weights: road_weights);
		if (path_followed != nil and path_followed.shape != nil) {
			cell[path_followed.shape.location] <- cell[path_followed.shape.location] + 10;
		}

		if (location = target) {
			target <- nil;
		} }

	aspect default {
		draw rectangle(4, 10) rotated_by (heading + 90) color: (#dodgerblue) depth: 3;
		draw rectangle(4, 6) rotated_by (heading + 90) color: (#dodgerblue) depth: 4;
	} }

species building {

	aspect default {
		draw shape color: darker(#darkgray).darker depth: rnd(10) + 2;
	}

}

species road {
	float capacity <- 1 + shape.perimeter / 30;
	int nb_people <- 0 update: length(people at_distance 1);
	float speed_coeff <- 1.0 update: exp(-nb_people / capacity) min: 0.1;
	int buffer <- 10;

	aspect default {
		draw (shape + 5) color: #white;
	}

}

species transporte_publico skills: [moving] {
// Destino del transporte
	point target;

	// Velocidad del transporte (más alta que people)
	float speed <- rnd(20) #km / #h + 10;

	// Capacidad máxima de pasajeros
	int capacidad <- 20;

	// Cantidad de personas dentro
	int pasajeros <- 0;

	// Probabilidad de detenerse en paradas
	float stop_proba <- 0.3;

	// Reflex para elegir un destino fijo o aleatorio
	reflex elegir_ruta when: target = nil {
		target <- any_location_in(one_of(building)); // Puede ser una parada o cualquier punto
	}

	// Reflex para moverse en la red de carreteras
	reflex mover when: target != nil {
		path ruta <- goto(target: target, on: road_network, recompute_path: false, return_path: true, move_weights: road_weights);

		// Si el transporte se movió, reduce menos contaminación que people
		if (ruta != nil and ruta.shape != nil) {
			cell[ruta.shape.location] <- cell[ruta.shape.location] + 5; // Menos impacto ambiental
		}

		if (location = target) {
			target <- nil;
		} }

		// Aspecto visual del transporte
	aspect default {
		draw rectangle(8, 16) rotated_by (heading + 90) color: #red depth: 2;
		draw rectangle(6, 12) rotated_by (heading + 90) color: #darkred depth: 3;
	} }

species moto skills: [moving] {
// Destino de la moto
	point target;

	// Velocidad de la moto (entre la de `people` y `transporte_publico`)
	float speed <- rnd(15) #km / #h + 5;

	// Probabilidad de hacer una parada
	float stop_proba <- 0.1;

	// Reflex para elegir un destino
	reflex elegir_destino when: target = nil {
		target <- any_location_in(one_of(building)); // Se dirige a un edificio aleatorio
	}

	// Reflex para moverse en la red de carreteras
	reflex mover when: target != nil {
		path ruta <- goto(target: target, on: road_network, recompute_path: false, return_path: true, move_weights: road_weights);

		// Si la moto se movió, genera algo de contaminación
		if (ruta != nil and ruta.shape != nil) {
			cell[ruta.shape.location] <- cell[ruta.shape.location] + 7; // Contamina menos que `people`
		}

		if (location = target) {
			target <- nil;
		} }

		// Aspecto visual de la moto
	aspect default {
		draw rectangle(3, 8) rotated_by (heading + 90) color: #lightgreen depth: 3;
		draw rectangle(2, 6) rotated_by (heading + 90) color: #lightgreen depth: 4;
	} }

	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
experiment traffic type: gui {
	parameter "Carros" var: carros category: "Agentes" min: 0 max: 800;
	parameter "Buses" var: buses category: "Agentes" min: 0 max: 800;
	parameter "Motos" var: motos category: "Agentes" min: 0 max: 800;
	float minimum_cycle_duration <- 0.01;
	list<rgb> pal <- palette([#black, #green, #yellow, #orange, #orange, #red, #red, #red]);
	map<rgb, string> pollutions <- [#green::"Good", #yellow::"Average", #orange::"Bad", #red::"Hazardous"];
	map<rgb, string> legends <- [rgb(darker(#darkgray).darker)::"Buildings", rgb(#dodgerblue)::"Carros", rgb(#red)::"Buses", rgb(#lightgreen)::"Motos", rgb(#white)::"Roads"];
	font text <- font("Arial", 14, #bold);
	font title <- font("Arial", 18, #bold);
	output synchronized: true {
		display carte type: 3d axes: false background: rgb(50, 50, 50) fullscreen: false toolbar: false {
			overlay position: {50 #px, 50 #px} size: {1 #px, 1 #px} background: #black border: #black rounded: false {
				
				
				draw "Pollution" at: {0, 0} anchor: #top_left color: #white font: title;
				draw "Promedio de Contaminacion: " + avg_pollution at: {3000, 100} color: #white font: title;
				draw "Tiempo transcurrido: " + total_duration + " ms" at: {3000, 200} color: #white font: title;
				
				float y <- 50 #px;
				draw rectangle(40 #px, 160 #px) at: {20 #px, y + 60 #px} wireframe: true color: #white;
				loop p over: reverse(pollutions.pairs) {
					draw square(40 #px) at: {20 #px, y} color: rgb(p.key, 0.6);
					draw p.value at: {60 #px, y} anchor: #left_center color: #white font: text;
					y <- y + 40 #px;
				}

				y <- y + 40 #px;
				draw "Legend" at: {0, y} anchor: #top_left color: #white font: title;
				y <- y + 50 #px;
				draw rectangle(40 #px, 120 #px) at: {20 #px, y + 40 #px} wireframe: true color: #white;
				loop p over: legends.pairs {
					draw square(40 #px) at: {20 #px, y} color: rgb(p.key, 0.8);
					draw p.value at: {60 #px, y} anchor: #left_center color: #white font: text;
					y <- y + 40 #px;
				}

			}

			light #ambient intensity: 128;
			camera 'default' location: {1254.041, 2938.6921, 1792.4286} target: {1258.8966, 1547.6862, 0.0};
			species road refresh: false;
			species building refresh: false;
			species people;
			species transporte_publico; // Agregarlo a la simulación
			species moto;
			mesh cell scale: 9 triangulation: true transparency: 0.4 smooth: 3 above: 0.8 color: pal;
		}

	}

}
