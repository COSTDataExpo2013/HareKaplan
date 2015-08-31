
var return_value = 0;

var outputBinding = new Shiny.OutputBinding();
$.extend(outputBinding, {
  find: function(scope) {
    return $(scope).find('.d3map');
  },
  renderValue: function(el, data) {
    do_stuff(el, data);
  }});
Shiny.outputBindings.register(outputBinding);

var inputBinding = new Shiny.InputBinding();
$.extend(inputBinding, {
  find: function(scope) {
    return $(scope).find('.d3map');
  },
  getValue: function(el) {
    return return_value;
  },
  subscribe: function(el, callback) {
    $(el).on("change.inputBinding", function(e) {
      callback();
    });
  },
});
Shiny.inputBindings.register(inputBinding);


function do_stuff(el, data) {
    //controls
    var metric = [{"var_name":"CCE", "disp_name": "Community Attachment", "max": 5},
                  {"var_name":"SOCIAL_O", "disp_name": "Social Offerings", "max": 3},
				          {"var_name":"OPENNESS", "disp_name": "Openness", "max": 3},
        				  {"var_name":"AESTHETI", "disp_name": "Aesthetics", "max": 3},
        				  {"var_name":"EDUCATIO", "disp_name": "Education", "max": 3},
        				  {"var_name":"BASIC_SE", "disp_name": "Basic Services", "max": 3},
        				  {"var_name":"LEADERSH", "disp_name": "Leadership", "max": 3},
        				  {"var_name":"ECONOMY", "disp_name": "Economy", "max": 3},
        				  {"var_name":"SAFETY", "disp_name": "Safety", "max": 3},
                  {"var_name":"SOCIAL_C", "disp_name": "Social Capital", "max": 3},
                  {"var_name":"INVOLVEM", "disp_name": "Civic Involvement", "max": 3}];


    //slider
    $('.irs-with-grid span').mouseup(function(){ setTimeout(function() { update_map(); update_graphs();}, 1000); });
    $('#aggregate').mouseup(function(){ setTimeout(function() { update_map(); update_graphs();}, 1000); });
    $('.checkbox span').mouseup(function(){ setTimeout(function() { update_map(); update_graphs();}, 1000); }); //wonky checkbox clicking

    //map
    var width = $('#d3io').width() - 180,
            height = width*0.520833,
            root, subset, corr_dat,
            title_qsb, graphs_qsb,
            scale_resp, scale_color,
            circ_selected,
            comms, comms_select,
            g, g_1, g_2, g_3,
            svg_1, svg_2, svg_3,
            metric_select = metric[0].var_name,
            year_select = 2008,
            level_select = {"level": "city", "index": 0, "value": "Aberdeen, SD"},
            color_scale, first = true,
            colors = ['red', 'white', 'blue'],
			      key;

    var projection = d3.geo.albersUsa()
        .scale(1.07858243*width)
        .translate([width / 2, height / 2]);

    var path = d3.geo.path()
        .projection(projection);

    var svg = d3.select(el).append("svg")
        .attr("tabindex", 1)
        .attr("width", width)
        .attr("height", height)
        .attr("class", "map");

    svg.append("rect")
        .attr("class", "background")
        .attr("width", width)
        .attr("height", height);

    g = svg.append("g");

    d3.json("data/us_states_5m.json", function(error, us) {
        g.append("g")
          .attr("id", "states")
        .selectAll("path")
          .data(topojson.feature(us, us.objects.states).features)
        .enter().append("path")
          .attr("d", path);

        g.append("path")
          .datum(topojson.mesh(us, us.objects.states, function(a, b) { return a !== b; }))
          .attr("id", "state-borders")
          .attr("d", path);

        if(data) {
            root = JSON.parse(data.data_json);
            console.log(root);
            corr_dat = JSON.parse(data.data_json_corr);

            update_map();
            update_graphs();
            update_table();
        }
    });

    //buttons
    $('<div id="buttons"></div>').insertAfter('svg.map');

    var mx_button = d3.select('#buttons').selectAll('div')
        .data(metric).enter()
        .append("div")
        .attr("class", "mx-button");

    mx_button.append("input")
            .attr("type", "radio")
            .attr("name", "mx")
            .attr("id", function(d, i) { return "button" + i; })
            .on("click", function(d) { click_metric(d); });

    mx_button.append("label")
            .attr("for", function(d, i){ return "button" + i; })
            .text(function(d){ return d.disp_name; })
            .attr("unselectable", "");

    mx_button.filter(function(d,i) { return(i === 0); }).selectAll("input")
        .attr("checked","");


    title_qsb = d3.select('.container-fluid').append('div')
        .attr("width", $('.container-fluid').width())
        .attr("class", "title_qsb");

    graphs_qsb = d3.select('.container-fluid').append('div')
        .attr("width", $('.container-fluid').width())
        .attr("height", $('.container-fluid').width()/3 + 60)
        .attr("class", "graphs_qsb");

    svg_1 = graphs_qsb.append("svg")
        .attr("width", $('.container-fluid').width()/3)
        .attr("height", $('.container-fluid').width()/3 + 60);

    g_1 = svg_1.append("g");

    svg_2 = graphs_qsb.append("svg")
        .attr("width", $('.container-fluid').width()/3)
        .attr("height", $('.container-fluid').width()/3 + 60);

    g_2 = svg_2.append("g");

    svg_3 = graphs_qsb.append("svg")
        .attr("width", $('.container-fluid').width()/3)
        .attr("height", $('.container-fluid').width()/3 + 60);

    g_3 = svg_3.append("g");

    function click_metric(d) {
		metric_select = d.var_name;
        update_map();
        update_graphs();
    }

    function update_map() {
        if(root) {
            if($('#aggregate').is(':checked')) {
                subset = root.all;
            } else {
                year_select = $('#year').val();
                subset = root[year_select];
            }

            scale_resp = d3.scale.pow().exponent(0.6)
                    .domain(d3.extent(subset, function(d){return d.TOTALRESP}))
                    .range([4,16]);

            scale_color = d3.scale.linear()
                        //.domain(d3.extent(subset, function(e){return e[metric_select]}))
                        .domain([1, 1 + 0.5*(metric.filter(function(e) {return e.var_name == metric_select;})[0].max - 1), metric.filter(function(e) {return e.var_name == metric_select;})[0].max])
                        .range(colors);

      			key_x = d3.scale.linear()
      				.domain([1, metric.filter(function(e) {return e.var_name == metric_select;})[0].max])
      				.range([0, 60]);

      			key_xAxis = d3.svg.axis()
      				.scale(key_x)
      				.orient("bottom")
      				.tickSize(13)
      				.tickFormat(d3.format("d"));

      			if(key) key.remove();

      			key = svg.append("g")
      				.attr("class", "key")
      				.attr("transform", "translate(0,15)");

      			key.selectAll("rect")
      				.data(pair(key_x.ticks(8)))
      			  .enter().append("rect")
      				.attr("height", 8)
      				.attr("x", function(d) { return key_x(d[0]); })
      				.attr("width", function(d) { return key_x(d[1]) - key_x(d[0]); })
      				.style("fill", function(d) { return scale_color(d[0]); });

      			key.call(key_xAxis).append("text")
      				.attr("class", "caption")
      				.attr("y", -6)
      				.text(metric.filter(function(e) {return e.var_name == metric_select;})[0].disp_name);

            comms = g.selectAll('circle.community').data(subset);

            comms.enter()
                .append('circle')
                .attr("class", "community")
                .attr("transform", function(d) {
                    return "translate(" + projection([d.lons, d.lats]) + ")";
                })
                .on("click", clicked)
                .append("title")
                .text(function(d){ return(d.QSB); });

            if(first) {
                comms.classed("selected", function(e, i) {return i === 0; });

                circ_selected = d3.selectAll("circle.selected").data()[0];

                comms_select = g.append("circle")
                .attr("class", "select")
                .attr("transform", "translate(" + projection([circ_selected.lons, circ_selected.lats]) + ")")
                .attr("r", scale_resp(circ_selected.TOTALRESP)*4)
                .style("fill", "none")
                .style("stroke", "black")
                .style("stroke-opacity", 1e-6)
                .style("stroke-width", 3);
            }
            first = false;

            comms.transition().duration(750)
                .style('fill', function(e){ return(scale_color(e[metric_select])); })
                .attr('r', function(e){ return(scale_resp(e.TOTALRESP)); })
                .style('stroke', '#7f7f7f')
                .style("stroke-width", 0.5);

      			comms.exit().remove();

            circ_selected = d3.selectAll("circle.selected").data()[0];
            if(circ_selected) {
                comms_select.transition().duration(750)
                  .attr('r', scale_resp(circ_selected.TOTALRESP))
                  .style("stroke-opacity", 1);
            }
        }
    }

    function clicked(d) {
        comms.classed("selected", false);
        comms.classed("selected", function(e) {return e.QSB == d.QSB; });

        update_table();

        g.selectAll("circle.select").remove();

        comms_select = g.append("circle")
            .attr("class", "select")
            .attr("transform", "translate(" + projection([d.lons, d.lats]) + ")")
            .attr("r", scale_resp(d.TOTALRESP)*4)
            .style("fill", "none")
            .style("stroke", "black")
            .style("stroke-opacity", 1e-6)
            .style("stroke-width", 3);

        comms_select.transition()
            .duration(750)
            .attr("r", scale_resp(d.TOTALRESP))
            .style("stroke-opacity", 1);

        circ_selected = d3.selectAll("circle.selected").data()[0];
        level_select = {"level": "city", "index": 0, "value": circ_selected.QSB };

        update_graphs();
        update_table();
    }

    function update_graphs() {
        if(circ_selected) {

            color_scale = d3.scale.category10();

            //graph 1
            var margin_1 = {top: 20, right: 20, bottom: 130, left: 40},
            g_width_1 =  $('.container-fluid').width()/3 - margin_1.left - margin_1.right,
            g_height_1 = $('.container-fluid').width()/3 + 60 - margin_1.top - margin_1.bottom;

            var x_1 = d3.scale.ordinal()
                .rangeRoundBands([0, g_width_1], 0.1, 0.5);

            var y_1 = d3.scale.linear()
                .range([g_height_1, 0]);

            var xAxis_1 = d3.svg.axis()
                .scale(x_1)
                .orient("bottom");

            var yAxis_1 = d3.svg.axis()
                .scale(y_1)
                .orient("left");

            var dataset_1 = [{ "x": circ_selected.QSB, "y": subset.filter(function(e){return e.QSB == circ_selected.QSB})[0][metric_select], "level": "city"},
                { "x": circ_selected.URBAN_GR, "y": d3.mean(subset.filter(function(e){return e.URBAN_GR == circ_selected.URBAN_GR}), function(k) {return k[metric_select]}), "level": "urbanicity"},
                { "x": circ_selected.Region, "y": d3.mean(subset.filter(function(e){return e.Region == circ_selected.Region}), function(k) {return k[metric_select]}), "level": "region"},
                { "x": "All Cities", "y": d3.mean(subset, function(k) {return k[metric_select]}), "level": "all"}];

            g_1.attr("transform", "translate(" + margin_1.left + "," + margin_1.top + ")");
            x_1.domain(dataset_1.map(function(d) { return d.x; }));
            y_1.domain([1, metric.filter(function(e) {return e.var_name == metric_select;})[0].max]);

            g_1.selectAll(".axis").remove();

            g_1.append("g")
              .attr("class", "x axis g1")
              .attr("transform", "translate(0," + g_height_1 + ")")
              .call(xAxis_1);

            g_1.selectAll(".x.axis").selectAll("text")
                .style("text-anchor", "start")
                .attr("transform", function(d) {
                    return "rotate(45)";
                });

            g_1.append("g")
              .attr("class", "y axis")
              .call(yAxis_1)
            .append("text")
              .attr("transform", "rotate(-90)")
              .attr("y", 6)
              .attr("dy", ".71em")
              .style("text-anchor", "end")
              .text(metric.filter(function(e) {return e.var_name == metric_select;})[0].disp_name);

            var bar = g_1.selectAll(".bar")
              .data(dataset_1);

            bar.enter().append("rect")
              .attr("class", "bar")
              .attr("fill", function(d,i){ return color_scale(i); })
              .on("click", click_bar);

            bar.transition().duration(750)
                .attr("x", function(d) { return x_1(d.x); })
                .attr("width", x_1.rangeBand())
                .attr("y", function(d) { return y_1(d.y); })
                .attr("height", function(d) { return g_height_1 - y_1(d.y); })
                .attr("opacity", function(d,i) {return i == level_select.index ? 1 : 0.5; });

    		bar.exit().remove();


    		//graph 2
        var margin_2 = {top: 20, right: 20, bottom: 130, left: 100},
        g_width_2 =  $('.container-fluid').width()/3 - margin_2.left - margin_2.right,
        g_height_2 = $('.container-fluid').width()/3 + 60 - margin_2.top - margin_2.bottom;

        var x_2 = d3.scale.linear()
                    .range([0, g_width_2]);

        var y_2 = d3.scale.ordinal()
            .rangePoints([g_height_2, 0], 1);

        var xAxis_2 = d3.svg.axis()
            .scale(x_2)
            .orient("bottom");

        var yAxis_2 = d3.svg.axis()
            .scale(y_2)
            .orient("left");

        var dataset_2 = subset.slice(0);
        dataset_2.sort(function(a, b){return a[metric_select] - b[metric_select]; });

        g_2.attr("transform", "translate(" + margin_2.left + "," + margin_2.top + ")");
        //x_2.domain([d3.min(dataset_2, function(d){return d[metric_select];}) - .5, d3.max(dataset_2, function(d){return d[metric_select];}) + .5]);
        x_2.domain([1, metric.filter(function(e) {return e.var_name == metric_select;})[0].max]);
        y_2.domain(dataset_2.map(function(d) { return d.QSB; }));

        g_2.selectAll(".axis").remove();

        g_2.append("g")
          .attr("class", "x axis")
          .attr("transform", "translate(0," + g_height_2 + ")")
          .call(xAxis_2)
        .append("text")
            .attr("transform", "translate(" + (g_width_2 / 2) + " , " + 2*margin_2.bottom/3 + ")")
            .style("text-anchor", "middle")
            .text(metric.filter(function(e) {return e.var_name == metric_select;})[0].disp_name);

        g_2.append("g")
          .attr("class", "y axis")
          .call(yAxis_2);

        var dot = g_2.selectAll("circle.dot")
          .data(dataset_2);

        dot.enter().append("circle")
          .attr("class", "dot")
          .on("click", clicked);

        dot.transition().duration(750)
            .attr("r", size_dots )
            .attr("fill", color_dots )
            .attr("cx", function(d) { return x_2(d[metric_select]); })
            .attr("cy", function(d) { return y_2(d.QSB); });

    		dot.exit().remove();

        dot.selectAll("title").remove();

        dot.append("title")
            .text(function(d){ return(d.QSB); });

        //graph 3
        var margin_3 = {top: 20, right: 20, bottom: 130, left: 40},
        g_width_3 =  $('.container-fluid').width()/3 - margin_3.left - margin_3.right,
        g_height_3 = $('.container-fluid').width()/3 + 60 - margin_3.top - margin_3.bottom;

        var x_3 = d3.scale.ordinal()
	          .rangePoints([0, g_width_3], 2);

        var y_3 = d3.scale.linear()
            .range([g_height_3, 0]);

        var xAxis_3 = d3.svg.axis()
            .scale(x_3)
            .orient("bottom");

        var yAxis_3 = d3.svg.axis()
            .scale(y_3)
            .orient("left");

        var year_val = $('#aggregate').is(':checked') ? "Aggregate" : year_select;
        var dataset_3 = corr_dat.filter(function(e) {return (e.Year == year_val) && (e.City == level_select.value); });
        var dataset_3_agg = corr_dat.filter(function(e) {return(e.Year == "Aggregate") && (e.City == "All Cities"); });

        var dataset_3_array = [];
        var dataset_3_agg_array = [];

        metric.forEach(function(d){
            if (d.var_name != "CCE") {
                dataset_3_array.push({"x": d.disp_name, "y": dataset_3[0][d.var_name], "var_name": d.var_name});
					      dataset_3_agg_array.push({"x": d.disp_name, "y": dataset_3_agg[0][d.var_name], "var_name": d.var_name});
              }
            });

            g_3.attr("transform", "translate(" + margin_3.left + "," + margin_3.top + ")");


            x_3.domain(dataset_3_array.map(function(d){ return d.x; }));
            y_3.domain([-1, 1]);

            g_3.selectAll(".axis").remove();

            g_3.append("g")
              .attr("class", "x axis")
              .attr("transform", "translate(0," + g_height_3 + ")")
              .call(xAxis_3)
            .append("text")
                .attr("transform", "translate(" + (g_width_3 / 2) + " , " + 2*margin_3.bottom/3 + ")")
                .style("text-anchor", "middle")
                .text(dataset_3[0].City + " - " + year_val);

            g_3.selectAll(".tick").selectAll("text")
                .style("text-anchor", "start")
                .attr("transform", function(d) {
                    return "rotate(45)";
                });


            g_3.append("g")
              .attr("class", "y axis")
              .call(yAxis_3)
             .append("text")
              .attr("transform", "rotate(-90)")
              .attr("y", 6)
              .attr("dy", ".71em")
              .style("text-anchor", "end")
              .text("Correlation to Community Attachment");


            if(g_3.selectAll("circle.agg").length > 0) {
        				g_3.selectAll("circle.agg")
        					.data(dataset_3_agg_array)
        					.enter().append("circle")
        					.attr("class", "agg")
        					.attr("fill", "grey")
        					.attr("r", 3)
        					.attr("cx", function(d) { return x_3(d.x); })
        					.attr("cy", function(d) { return y_3(d.y); })
        					.attr("opacity", 0.5);
                }

            var dot_3 = g_3.selectAll("circle.dot")
              .data(dataset_3_array);

            dot_3.enter().append("circle")
              .attr("class", "dot")
              .attr("fill", function(d, i){ return color_scale(i); })
              .on("click", click_dot_3)
              .append("title")
                .text(function(d){ return(d.x); });

            dot_3.transition().duration(750)
                .attr("r", function(d) { return d.var_name == metric_select ? 8 : 5; })
                .attr("cx", function(d) { return x_3(d.x); })
                .attr("cy", function(d) { return y_3(d.y); });

	          dot_3.exit().remove();

        }
    }

    function click_bar(d, i) {
        level_select = {"level": d.level,"index": i, "value": d.x };
        update_graphs();
    }

    function click_dot_3(d) {
		mx_button.filter(function(e) { return(d.var_name == e.var_name); }).selectAll("input")
            .attr("checked", "");

        click_metric(d);
    }

    function color_dots(d) {
        var color;

        if(level_select.level == "city") {
            if(d.QSB == circ_selected.QSB) {
                color = "#1f77b4";
            } else {
                color = "grey";
            }
        } else if(level_select.level == "urbanicity") {
            if(d.QSB == circ_selected.QSB) {
                color = "#1f77b4";
            } else if(d.URBAN_GR == level_select.value) {
                color = "#ff7f0e";
            } else {
                color = "grey";
            }
        } else if(level_select.level == "region") {
            if(d.QSB == circ_selected.QSB) {
                color = "#1f77b4";
            } else if(d.Region == level_select.value) {
                color = "#2ca02c";
            } else {
                color = "grey";
            }
        } else {
            if(d.QSB == circ_selected.QSB) {
                color = "#1f77b4";
            } else {
                color = "#d62728";
            }
        }
        return color;
    }

    function size_dots(d) {
        var size;

        if(level_select.level == "city") {
            if(d.QSB == circ_selected.QSB) {
                size = 4;
            } else {
                size = 2;
            }
        } else if(level_select.level == "urbanicity") {
            if(d.QSB == circ_selected.QSB || d.URBAN_GR == level_select.value) {
                size = 4;
            } else {
                size = 2;
            }
        } else if(level_select.level == "region") {
            if(d.QSB == circ_selected.QSB || d.Region == level_select.value) {
                size = 4;
            } else {
                size = 2;
            }
        } else {
            size = 4;
        }
        return size;
    }

    function update_table() {
        if(circ_selected) {
            title_qsb.selectAll(".infocontainer").remove();

            var container = title_qsb.append("div").attr("class", "infocontainer");
            var dataset = root.all.filter(function(e){ return e.QSB == circ_selected.QSB; })

            container.append("span").html("<span style='font-weight:bold; font-size:24px; margin-right: 5px'>" + dataset[0].QSB + "</span>" +
                                          "<strong> Urbanicity: </strong>" + dataset[0].Urbanicity + " | " +
                                          "<strong> Region: </strong>" + dataset[0].Region + " | " +
                                          "<strong> Incorporated: </strong>" + dataset[0].Incorporated + " | " +
                                          "<strong> Population: </strong>" + dataset[0].Population + " | " +
                                          "<strong> Unemployment: </strong>" + dataset[0].Unemployment)

        }
    }

  	function pair(array) {
  	  return array.slice(1).map(function(b, i) {
  		return [array[i], b];
  	  });
  	}
}
