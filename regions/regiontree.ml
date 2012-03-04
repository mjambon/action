type 'a t

type region_id = int

type 'a region_data = {
  mutable region_elements : 'a Prioqueue.t;
}

type 'a region = {
  region_id : region_id;
  region_data : 'a region_data;
  region_tree : 'a t;
}

type elt_data

type 'a elt = {
  mutable elt_score : float;
  mutable elt_region : 'a region;
  elt_data : elt_data;
  elt_contents : 'a;
}

let create n 
