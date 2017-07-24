
package com.github.knowrob_sherpa_queries;
import org.ros.message.Duration;
import java.util.ArrayList;
import java.util.Map;
import java.util.List;
import java.io.*;
import org.apache.commons.logging.Log;
import java.util.concurrent.ConcurrentHashMap;
import org.ros.exception.RemoteException;
import org.ros.exception.RosRuntimeException;
import org.ros.exception.ServiceNotFoundException;
import org.ros.namespace.GraphName;
import org.ros.concurrent.CancellableLoop;
import org.ros.node.AbstractNodeMain;
import org.ros.node.ConnectedNode;
import org.ros.node.NodeMain;
import visualization_msgs.Marker;
import visualization_msgs.MarkerArray;
import geometry_msgs.Pose;
import org.ros.node.topic.Publisher;
import java.util.HashMap;
import java.text.DecimalFormat;
import java.util.LinkedList;


// callDown(A,B):-
// sherpa_interface(SHERPA),
// get_objects_by_type(A, Names),
// get_objects_by_pose(A, Poses),
// jpl_list_to_array(Names,Name),
// jpl_list_to_array(Poses,Pose),
// jpl_call(SHERPA,'slopeDown',[Name,Pose],C),
// jpl_array_to_list(C,D),
// foreach(member(X,D), add_arrow(X,_)).

// callUp(A,B):-
// sherpa_interface(SHERPA),
// get_objects_by_type(A, Names),
// get_objects_by_pose(A, Poses),
// jpl_list_to_array(Names,Name),
// jpl_list_to_array(Poses,Pose),
// jpl_call(SHERPA,'slopeUp',[Name,Pose],C),
// jpl_array_to_list(C,D),
// foreach(member(X,D), add_arrow(X,_)).


public class VisualizationMarker extends AbstractNodeMain{
	private static final String HTML_RED = "ff0000";

	Publisher<MarkerArray> pub;
	
	Publisher<Pose> cam_pub;
	
	Publisher<std_msgs.String> text_pub;
	
    //	Publisher<data_vis_msgs.Speech> speech_pub;

	ConnectedNode node;

	static String reference_frame = null; 

	/**
	 * Store the markers to be published
	 */
	protected Map<String, Marker> markers;
	/**
	 * Store all added markers (for highlighting)
	 */
	protected Map<String, Marker> markersCache;

	/**
	 * Stores original colors of highlighted objects
	 */
    protected Map<String, float[]> highlighted;
    
    protected Map<String, float[]> mapObjects;
    

	/**
	 * Counter for marker IDs
	 */
	private static int id = 0;

	protected Map<String, List<String>> trajectories;

	/**
	 * Stores the set of joints which are available from the XSens motion
	 * capturing suite.
	 */
    //	private Map<String, Skeleton> agentSkeletons;

	/**
	 * Logger of ROS node.
	 */
	private Log log;

	private static VisualizationMarker instance = null;


       public static VisualizationMarker get() {
  		if(instance==null) instance = new VisualizationMarker();
  		return instance;
  	}

  	// /**
  	//  * Constructor. Starts the marker publisher in a parallel thread.
  	//  */
  	 private VisualizationMarker() {
  	 	markers =  new ConcurrentHashMap<String, Marker>(8, 0.9f, 1);
  	 	markersCache =  new ConcurrentHashMap<String, Marker>(8, 0.9f, 1);
		mapObjects = new HashMap<String, float[]>();
  	// 	highlighted = new ConcurrentHashMap<String, float[]>(8, 0.9f, 1);
  	// 	trajectories = new HashMap<String, List<String>>();
  	// //	agentSkeletons = new HashMap<String, Skeleton>();
  	 }

        @Override
	public void onStart(final ConnectedNode connectedNode) {
	    node = connectedNode;
	    pub = connectedNode.newPublisher("/visualization_marker_array",visualization_msgs.MarkerArray._TYPE);
	}

    public void clear() {
	try {
	    // wait for node to be ready
	    waitForNode();
	    
	    final MarkerArray arr = pub.newMessage();
	    synchronized (markersCache) {
		for(Marker m : markersCache.values()) {
		    m.setAction(Marker.DELETE);
		    arr.getMarkers().add(m);
		}
		markersCache.clear();
	    }
	    synchronized (markers) {
		markers.clear();
	    }
	  
	    pub.publish(arr);
	}
	catch(Exception e) {
	    e.printStackTrace();
		}
}

     public void publishMarkers() {
		try {
		    synchronized (markers) {
			MarkerArray arr = pub.newMessage();
			
			for(Marker mrk : markers.values()) {
			    arr.getMarkers().add(mrk);
			}
			pub.publish(arr);
			markers.clear();
		    }
		} catch (Exception e) {
			e.printStackTrace();
		}
	}


  	 @Override
  	public GraphName getDefaultNodeName() {
  	 	return GraphName.of("knowrob_sherpa_queries");
  	 }

 public String addUnderscoreArrow(float[] pose, float[] dim){
     return "hi";
 }

  /**
  	 * Show pointer in visualization canvas.
  	 *
  	 * @param tfSlink TF identifier, source link
  	 * @param tfElink TF identifier, target link
  	 * @param starttime OWL identifier of a timepoint instance
  	 * @param markertype marker type id (see ROS Marker message)
  	 */

    
// add_arrow(Obj,FIN) :-
// sherpa_interface(SHERPA),
// current_object_pose(Obj,Pose),
// jpl_list_to_array(Pose,ARR),
// jpl_call(SHERPA,'addArrow',[ARR],FIN).
    //public String addArrow(float[] pose) {
    public String addArrow(float[] pose, float[] dim){
	final Marker m;
	m = createMarker();
	m.setType(Marker.ARROW);
	m.setMeshUseEmbeddedMaterials(true);
	float x = pose[0];
	float y = pose[1];
	float z = pose[2] + dim[2] + 20;
	m.getPose().getPosition().setX(x);
	m.getPose().getPosition().setY(y);
	m.getPose().getPosition().setZ(z);
	m.getPose().getOrientation().setW(1);
	m.getPose().getOrientation().setX(0);
	m.getPose().getOrientation().setY(1);
	m.getPose().getOrientation().setZ(0);	
	m.getScale().setX(5);
	m.getScale().setY(5);
	m.getScale().setZ(20.0);
	m.getColor().setR(1);
	m.getColor().setG(0);
	m.getColor().setB(0);
	m.getColor().setA(1.0f);
  	 	 //add marker to map
	final StringBuilder identifier = new StringBuilder();
	identifier.append(m.getNs()).append('_').append(m.getId());
	synchronized(markers) {
	    markers.put(identifier.toString(),m);
	}
	
	synchronized(markersCache) {
	    markersCache.put(identifier.toString(),m);
	}
	publishMarkers();
	
	return "Cool that worked";
    }


    public void visualizeAreas(float[] pose)
    {
	final Marker m;
	m = createMarker();
	m.setType(Marker.CUBE);
	m.setMeshUseEmbeddedMaterials(true);
	float x = pose[0];
	float y = pose[1];
	float z = pose[2];
	m.getPose().getPosition().setX(x);
	m.getPose().getPosition().setY(y);
	m.getPose().getPosition().setZ(z);
	m.getPose().getOrientation().setW(pose[3]);
	m.getPose().getOrientation().setX(pose[4]);
	m.getPose().getOrientation().setY(pose[5]);
	m.getPose().getOrientation().setZ(pose[6]);	
	m.getScale().setZ(10.0);
	m.getScale().setY(3.0);
	m.getScale().setX(3.0);
	m.getColor().setR(1);
	m.getColor().setG(0);
	m.getColor().setB(0);
	m.getColor().setA(0.7f);
  	 	 //add marker to map
	final StringBuilder identifier = new StringBuilder();
	identifier.append(m.getNs()).append('_').append(m.getId());
	synchronized(markers) {
	    markers.put(identifier.toString(),m);
	}
	
	synchronized(markersCache) {
	    markersCache.put(identifier.toString(),m);
	}
	publishMarkers();
    }
    public String removeMapObject(String A){
	return "checked";
    }

    public void addNameText(String name, float[] transform)
    {
	final Marker m;
	m = createMarker();
	m.setType(Marker.TEXT_VIEW_FACING);
	m.setMeshUseEmbeddedMaterials(true);
	m.setText(name);
	float x = transform[0];
	float y = transform[1];
	float z = transform[2]+3;
	m.getPose().getPosition().setX(x);
	m.getPose().getPosition().setY(y);
	m.getPose().getPosition().setZ(z);
	m.getPose().getOrientation().setW(1);
	m.getPose().getOrientation().setX(0);
	m.getPose().getOrientation().setY(0);
	m.getPose().getOrientation().setZ(0);	
	m.getScale().setZ(4.0);
	m.getColor().setR(1);
	m.getColor().setG(1);
	m.getColor().setB(0);
	m.getColor().setA(1.0f);
  	 	 //add marker to map
	final StringBuilder identifier = new StringBuilder();
	identifier.append(m.getNs()).append('_').append(m.getId());
	synchronized(markers) {
	    markers.put(identifier.toString(),m);
	}
	
	synchronized(markersCache) {
	    markersCache.put(identifier.toString(),m);
	}
	publishMarkers();

    }
       
    public String addText(float[] pose, float[] dim, String objname)
    {
	final Marker m;
	m = createMarker();
	m.setType(Marker.TEXT_VIEW_FACING);
	m.setMeshUseEmbeddedMaterials(true);
	String[] name = new String[2]; 

	if(objname.contains("#"))
	   {
	       name = objname.split("#");
	   }else
		{
		    name = objname.split(":");
		}
	m.setText(name[1]);
	float x = pose[0];
	float y = pose[1];
	float z = pose[2] + dim[2] + 25;
	m.getPose().getPosition().setX(x);
	m.getPose().getPosition().setY(y);
	m.getPose().getPosition().setZ(z);
	m.getPose().getOrientation().setW(1);
	m.getPose().getOrientation().setX(0);
	m.getPose().getOrientation().setY(0);
	m.getPose().getOrientation().setZ(0);	
	m.getScale().setZ(20.0);
	m.getColor().setR(1);
	m.getColor().setG(0);
	m.getColor().setB(0);
	m.getColor().setA(1.0f);
  	 	 //add marker to map
	final StringBuilder identifier = new StringBuilder();
	identifier.append(m.getNs()).append('_').append(m.getId());
	synchronized(markers) {
	    markers.put(identifier.toString(),m);
	}
	
	synchronized(markersCache) {
	    markersCache.put(identifier.toString(),m);
	}
	publishMarkers();
	return "";
    }

    public void getDetectedObjects(float[][] transforms, String[] objs, float[][] dimensions)
    {
	float[] arr = new float[7];

	for(int i = 0; i < transforms.length; i++)
	    {
		arr[0] = transforms[i][3];
		arr[1] = transforms[i][7];
		arr[2] = transforms[i][11];
		arr[3] = 0;
		arr[4] = 0;
		arr[5] = 0;
		arr[6] = 1;
		addTraces(arr, objs, dimensions);
	    }
	//return arr; 

    }

    public void addTraces(float[] pose, String[] objs, float[][] dimensions)
    {
	float[] min = new float[3];
	float[] max = new float[3];

	System.out.println("addTraces");
	for(int i=0; i <= 770; i= i+2)
	    {

		float x = pose[0];
		float y = pose[1];
		float z = pose[2] - i;

		for(int j=0; j < objs.length; j= j+2)
		    {
			min[0] = dimensions[j][0];
			min[1] = dimensions[j][1];
			min[2] = dimensions[j][2];
			max[0] = dimensions[j+1][0];
			max[1] = dimensions[j+1][1];
			max[2] = dimensions[j+1][2];
			
			if(checkValueInBoundingBox(min, max, pose))
			   {
			       System.out.println(objs[j]);
			       float r = 0.0f;
			       float g = 1.0f;
			       float b = 0.0f;
			       addRayTracingMarker(pose, r, g, b);
			   }
		    }

	// final Marker m;
	
	// m = createMarker();
	// m.setType(Marker.CYLINDER);
	// m.setMeshUseEmbeddedMaterials(true);
	
	// m.getPose().getPosition().setX(x);
	// m.getPose().getPosition().setY(y);
	// m.getPose().getPosition().setZ(z);
	// m.getPose().getOrientation().setW(1);
	// m.getPose().getOrientation().setX(0);
	// m.getPose().getOrientation().setY(1);
	// m.getPose().getOrientation().setZ(0);	
	// m.getScale().setX(2);
	// m.getScale().setY(2);
	// m.getScale().setZ(2.0);
	// m.getColor().setR(1);
	// m.getColor().setG(0);
	// m.getColor().setB(0);
	// m.getColor().setA(0.7f);
	// //add marker to map
	// final StringBuilder identifier = new StringBuilder();
	// identifier.append(m.getNs()).append('_').append(m.getId());
	// synchronized(markers) {
	//     markers.put(identifier.toString(),m);
	// }
	
	// synchronized(markersCache) {
	//     markersCache.put(identifier.toString(),m);
	// }
	// publishMarkers();
	// System.out.println("test");
	//     }
	    }
	
    }


    public float[][] getObjectsMinMax(String[] objs, float[][] trans, float[][] dims)
    {
	int doppelt = objs.length + objs.length;
	float[][] arr = new float[doppelt][3];
	for(int i = 0; i < objs.length; i++)
	    {
		float x = dims[i][0];
		float y = dims[i][1];
		float z = dims[i][2];
		float minx = trans[i][0] - (x / 2);
		float miny = trans[i][1] - (y / 2);
		float minz = trans[i][2];
		float maxx = trans[i][0] + (x / 2);
		float maxy = trans[i][1] + (y / 2);
		float maxz = trans[i][2] + z;
		arr[i][0] = minx;
		arr[i][1] = miny;
		arr[i][2] = minz;
		arr[i+1][0] = maxx;
		arr[i+1][1] = maxy;
		arr[i+1][2] = maxz;
	    }

	return arr;

    }

    public void addRayTracingMarker(float[] pose, float r, float g, float b)
    {
	float xyz = 0.0f;
	if( g != 1.0f)
	    {
		xyz = 20.0f;
	    }
	final Marker m;
	m = createMarker();
	m.setType(Marker.CUBE);
	m.setMeshUseEmbeddedMaterials(true);
	float x = pose[0];
	float y = pose[1];
	float z = pose[2] + xyz;
	m.getPose().getPosition().setX(x);
	m.getPose().getPosition().setY(y);
	m.getPose().getPosition().setZ(z);
	m.getPose().getOrientation().setW(1);
	m.getPose().getOrientation().setX(0);
	m.getPose().getOrientation().setY(1);
	m.getPose().getOrientation().setZ(0);	
	m.getScale().setZ(20.0);
	m.getScale().setY(20.0);
	m.getScale().setX(20.0);
	m.getColor().setR(r);
	m.getColor().setG(g);
	m.getColor().setB(b);
	m.getColor().setA(0.7f);
  	 	 //add marker to map
	final StringBuilder identifier = new StringBuilder();
	identifier.append(m.getNs()).append('_').append(m.getId());
	synchronized(markers) {
	    markers.put(identifier.toString(),m);
	}
	
	synchronized(markersCache) {
	    markersCache.put(identifier.toString(),m);
	}
	publishMarkers();
    }
    
    public Marker createMarker() {
	waitForNode();
	Marker m = node.getTopicMessageFactory().newFromType(visualization_msgs.Marker._TYPE);
	m.setNs("knowrob_sherpa_queries");
	m.setId(id++);
	m.setAction(Marker.ADD);
	m.setLifetime(new Duration());
	m.getColor().setR(1.0f);
	m.getColor().setG(0.0f);
	m.getColor().setB(0.0f);
	m.getColor().setA(1.0f);
	return m;
    }

    private void waitForNode() {
	// wait for node to be ready
	try {
	    while(node == null || pub == null) {
		Thread.sleep(200);
	    }
	} catch (InterruptedException e) {
	    e.printStackTrace();
	}
    }
    

    public void visualizeBBoxes(float[] pose, float[] dim)
    {

	final Marker m;
	m = createMarker();
	m.setType(Marker.CUBE);
	m.setMeshUseEmbeddedMaterials(true);
	float x = pose[0];
	float y = pose[1];
	float z = pose[2];
	m.getPose().getPosition().setX(x);
	m.getPose().getPosition().setY(y);
	m.getPose().getPosition().setZ(z);
	m.getPose().getOrientation().setW(pose[3]);
	m.getPose().getOrientation().setX(pose[4]);
	m.getPose().getOrientation().setY(pose[5]);
	m.getPose().getOrientation().setZ(pose[6]);	
	m.getScale().setZ(dim[2]);
	m.getScale().setY(dim[1]);
	m.getScale().setX(dim[0]);
	m.getColor().setR(1);
	m.getColor().setG(0);
	m.getColor().setB(0);
	m.getColor().setA(0.8f);
  	 	 //add marker to map
	final StringBuilder identifier = new StringBuilder();
	identifier.append(m.getNs()).append('_').append(m.getId());
	synchronized(markers) {
	    markers.put(identifier.toString(),m);
	}
	
	synchronized(markersCache) {
	    markersCache.put(identifier.toString(),m);
	}
	publishMarkers();

    }
    public void detectedObject(float[] pose){
	final Marker m;
	m = createMarker();
	m.setType(Marker.ARROW);
	m.setMeshUseEmbeddedMaterials(true);
	float x = pose[0];
	float y = pose[1];
	float z = pose[2] + 5;
	m.getPose().getPosition().setX(x);
	m.getPose().getPosition().setY(y);
	m.getPose().getPosition().setZ(z);
	m.getPose().getOrientation().setW(1);
	m.getPose().getOrientation().setX(0);
	m.getPose().getOrientation().setY(1);
	m.getPose().getOrientation().setZ(0);	
	m.getScale().setX(5);
	m.getScale().setY(5);
	m.getScale().setZ(10.0);
	m.getColor().setR(1);
	m.getColor().setG(0);
	m.getColor().setB(0);
	m.getColor().setA(1.0f);
	
	//add marker to map
	final StringBuilder identifier = new StringBuilder();
	identifier.append(m.getNs()).append('_').append(m.getId());
	synchronized(markers) {
	    markers.put(identifier.toString(),m);
	}
	synchronized(markersCache) {
	    markersCache.put(identifier.toString(),m);
	}					
	publishMarkers();
	//	 return "Cool that worked2";
	
    }
    
    public void slopeUp(String[] names, float[][] nums)
    {
	//String[] arr = new String[3];

	float value = nums[0][2];
	float test;
	
	for(int i =1; i < names.length; i++)
	    {
		if(nums[i][2] >= 0 && value >= 0)
		    {
			test = nums[i][2] - value;
			if(test >= 3)
			    {
				float[] num = new float[3];
				num[0] = nums[i][0];
				num[1] = nums[i][1];
				num[2] = nums[i][2];
				addSlopeArrow(num, 0.0f, 0.0f, 1.0f, 1.0f);	    
			    }
			value = nums[i][2];
		    }else 
		    if(nums[i][2] >= 0 && value < 0)
			{
			    test = nums[i][2] + value;
			    if(test >= 3)
				{
				    float[] num = new float[3];
				    num[0] = nums[i][0];
				    num[1] = nums[i][1];
				    num[2] = nums[i][2];
				    
				    addSlopeArrow(num, 0.0f, 0.0f, 1.0f, 1.0f);
				}
			    value = nums[i][2];
			}else
			if(nums[i][2] < 0 && value >= 0)
			    {
				test = nums[i][2] + value;
				if(test >= 3)
				    {
					
					float[] num = new float[3];
					num[0] = nums[i][0];
					num[1] = nums[i][1];
					num[2] = nums[i][2];
					
					addSlopeArrow(num, 0.0f, 0.0f, 1.0f, 1.0f);
				    }
				value = nums[i][2];
			    }else
			    if(nums[i][2] < 0 && value < 0)
				{
				    float tmp = nums[i][2] * (- 1);
				    float tmp1 = nums[i][2] * (- 1);
				    test  = tmp - tmp1;
				    if(test >= 3)
					{
					    
					    float[] num = new float[3];
					    num[0] = nums[i][0];
					    num[1] = nums[i][1];
					    num[2] = nums[i][2];
					    
					    addSlopeArrow(num, 0.0f, 0.0f, 1.0f, 1.0f);
					}
				    value = nums[i][2];
				}
	    }
	slopeDown(names,nums);
	slopeStrong(names,nums);
	
    }
    
 public void slopeStrong(String[] names, float[][] nums)
    {
	//String[] arr = new String[3];
	float value = nums[0][2];
	float test;
	
	for(int i =1; i < names.length; i++)
	    {
		if(nums[i][2] >= 0 && value >= 0)
		    {
			test = nums[i][2] - value;
			if(test >= 5)
			    {
				float[] num = new float[3];
				num[0] = nums[i][0];
				num[1] = nums[i][1];
				num[2] = nums[i][2];
				addSlopeArrow(num, 1.0f, 0.0f, 1.0f, 1.0f);	    
			    }
			value = nums[i][2];
		    }else 
		    if(nums[i][2] >= 0 && value < 0)
			{
			    test = nums[i][2] + value;
			    if(test >= 5)
				{
				    float[] num = new float[3];
				    num[0] = nums[i][0];
				    num[1] = nums[i][1];
				    num[2] = nums[i][2];
				    
				    addSlopeArrow(num, 1.0f, 0.0f, 1.0f, 1.0f);
				}
			    value = nums[i][2];
			}else
			if(nums[i][2] < 0 && value >= 0)
			    {
				test = nums[i][2] + value;
				if(test >= 5)
				    {
					
					float[] num = new float[3];
					num[0] = nums[i][0];
					num[1] = nums[i][1];
					num[2] = nums[i][2];
					
					addSlopeArrow(num, 1.0f, 0.0f, 1.0f, 1.0f);
				    }
				value = nums[i][2];
			    }else
			    if(nums[i][2] < 0 && value < 0)
				{
				    float tmp = nums[i][2] * (- 1);
				    float tmp1 = nums[i][2] * (- 1);
				    test  = tmp - tmp1;
				    if(test >= 5)
					{
					    
					    float[] num = new float[3];
					    num[0] = nums[i][0];
					    num[1] = nums[i][1];
					    num[2] = nums[i][2];
					    
					    addSlopeArrow(num, 1.0f, 0.0f, 1.0f, 1.0f);
					}
				    value = nums[i][2];
				}
	    }
	
    }
    
    
    public void addSlopeArrow(float[] num, float r, float g, float b, float a)
    {
	final Marker m;
	m = createMarker();
	m.setType(Marker.ARROW);
	m.setMeshUseEmbeddedMaterials(true);
	float x = num[0];
	float y = num[1];
	float z = num[2] + 15;
	m.getPose().getPosition().setX(x);
	m.getPose().getPosition().setY(y);
	m.getPose().getPosition().setZ(z);
	m.getPose().getOrientation().setW(1);
	m.getPose().getOrientation().setX(0);
	m.getPose().getOrientation().setY(1);
	m.getPose().getOrientation().setZ(0);	
	m.getScale().setX(5);
	m.getScale().setY(5);
	m.getScale().setZ(20.0);
	m.getColor().setR(r);
	m.getColor().setG(g);
	m.getColor().setB(b);
	m.getColor().setA(a);
	
	//add marker to map
	final StringBuilder identifier = new StringBuilder();
	identifier.append(m.getNs()).append('_').append(m.getId());
	
	synchronized(markers) {
	    markers.put(identifier.toString(),m);
	}
	
	synchronized(markersCache) {
	    markersCache.put(identifier.toString(),m);
	}
	
	publishMarkers();
	
    }
    
    public void slopeDown(String[] names, float[][] nums)
    {

	float value = nums[0][2];
	float test;
	for(int i =1; i < names.length; i++)
	    {
		if(nums[i][2] >= 0 && value >= 0)
		    {
			test = nums[i][2] - value;
			if( test <= -3)
			    {
				float[] num = new float[3];
				num[0] = nums[i][0];
				num[1] = nums[i][1];
				num[2] = nums[i][2];
				
				addSlopeArrow(num,0.0f, 0.0f, 1.0f, 1.0f);
			    }
			value = nums[i][2];
		    }else 
		    if(nums[i][2] >= 0 && value < 0)
			{
			    
			    test = nums[i][2] + value;
			    if(test <= -3)
				{
				    float[] num = new float[3];
				    num[0] = nums[i][0];
				    num[1] = nums[i][1];
				    num[2] = nums[i][2];
				    
				    addSlopeArrow(num,0.0f, 0.0f, 1.0f, 1.0f);
				}
			    value = nums[i][2];
			}else
			if(nums[i][2] < 0 && value >= 0)
			    {
				test = nums[i][2] + value;
				if(test <= -3)
				    {
					float[] num = new float[3];
					num[0] = nums[i][0];
					num[1] = nums[i][1];
					num[2] = nums[i][2];
					
					addSlopeArrow(num,0.0f, 0.0f, 1.0f, 1.0f);
				    }
				value = nums[i][2];
			    }else
			    if(nums[i][2] < 0 && value < 0)
				{
				    float tmp = nums[i][2] * (- 1);
				    float tmp1 = nums[i][2] * (- 1);
				    test  = tmp - tmp1;
				    if(test <= -3)
					{
					    float[] num = new float[3];
					    num[0] = nums[i][0];
					    num[1] = nums[i][1];
					    num[2] = nums[i][2];
					    
					    addSlopeArrow(num,0.0f, 0.0f, 1.0f, 1.0f);
					}
				    value = nums[i][2];
				}
	    }	
    }

    public boolean checkValueInBoundingBox(float[] min, float[] max, float[] point)
    {
	if(point[0] >= min[0] &&
	   point[0] <= max[0] &&
	   point[1] >= min[1] &&
	   point[1] <= max[1] &&
	   point[2] >= min[2] &&
	   point[2] <= max[2])
	    return true;
	return false;
    }


    public void visualizeLocation(float[] transform)
    {
	final Marker m;
	m = createMarker();
	m.setType(Marker.ARROW);
	m.setMeshUseEmbeddedMaterials(true);
	float x = transform[0];
	float y = transform[1];
	float z = transform[2];
	m.getPose().getPosition().setX(x);
	m.getPose().getPosition().setY(y);
	m.getPose().getPosition().setZ(z);
	m.getPose().getOrientation().setW(1);
	m.getPose().getOrientation().setX(0);
	m.getPose().getOrientation().setY(-1);
	m.getPose().getOrientation().setZ(0);	
	m.getScale().setX(2);
	m.getScale().setY(2);
	m.getScale().setZ(10.0);
	m.getColor().setR(1);
	m.getColor().setG(1);
	m.getColor().setB(0);
	m.getColor().setA(1.0f);
  	 	 //add marker to map
	final StringBuilder identifier = new StringBuilder();
	identifier.append(m.getNs()).append('_').append(m.getId());
	synchronized(markers) {
	    markers.put(identifier.toString(),m);
	}
	
	synchronized(markersCache) {
	    markersCache.put(identifier.toString(),m);
	}
	publishMarkers();

    }

    public String[] Roadneighbours()
    {
	String[] newArray = {"http://knowrob.org/kb/unreal_log.owl#RoadSegment_ULIF",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_CL1P",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_T0KO",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_FdGR",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test001",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_T7F4",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test002",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_nTnX",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test003",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_0c7k",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_8FyA",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_douW",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test005",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_ymF2",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test088",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_MB6X",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test087",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test006",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_O5lp",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_a2li",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test007",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_oywf",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test008",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_kN9i",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test009",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_2roE",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_f7dB",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_nnhu",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test010",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_Kc95",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test011",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_XDfk",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test012",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_yJc6",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test013",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_vBzx",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_x7LV",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test015",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_SMNT",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test016",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_163D",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test014",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_xIQV",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test017",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_k5vM",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test018",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_iQwa",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test019",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_GkTZ",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test020",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_ssvG",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test021",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_FHVo",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_7qsH",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test029",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_cv2v",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test022",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_2LoX",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test023",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_0me3",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test024",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_TAgO",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test025",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_p2SV",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test026",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test101",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_64da",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test027",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_FRQh",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test030",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_uMpz",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test031",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_7TTJ",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test040",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_2YqG",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test032",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_Axra",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test033",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_9C91",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test034",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_YvsT",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test035",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_7xJQ",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test036",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_WLhv",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test037",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_tvW6",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test038",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test039",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_ve9S",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test041",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_3f9w",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test042",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_rSuo",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test044",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_5hiP",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test045",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_rHzq",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test061",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_CBGS",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_dAFW",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test064",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_aucQ",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test065",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_mjat",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test060",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_N0uP",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test059",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_soCa",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test066",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_64gj",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_ZYLx",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test050",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_aIWx",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test051",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_iH5x",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test052",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_RxrV",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test053",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_HhQm",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test054",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_gSPp",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test055",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_pNQD",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test056",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_xohv",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test057",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_q3J4",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test058",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_Iqxk",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test070",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_YCAT",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test071",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_RoH1",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test072",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_Riyb",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test073",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_q2KZ",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test074",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_zDX8",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test075",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_SEVd",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test076",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_j7Pp",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test077",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_YL5x",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test078",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_WguZ",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test079",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_eXjK",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test080",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_6uk7",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test081",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_y2wG",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test082",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_K2HE",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test083",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_m0e6",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test084",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_3Nai",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test085",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test086",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_R2rM",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_o1yX",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_test100",
			     "http://knowrob.org/kb/unreal_log.owl#RoadSegment_IhAY"};
	return newArray;
    }
    
    public String[] getAllTimepoints(String link, String startTime, String endTime, double interval)
    {
	String tflink_ = (link.startsWith("/") ? link : "/"+link);
	ArrayList<String> list = new ArrayList<String>();
	double t0 = parseTime_d(startTime);
	double t1 = parseTime_d(endTime);
	double length = 0;

	for (double i = t0; i <= t1; i += interval) 
	    {
		String timepoint = "timepoint_" + new DecimalFormat("###.###").format(i);
		list.add(timepoint);
	    }
	String[] array = new String[list.size()];
	array = list.toArray(array); 
	
	return array;

    }

	/**
	 * Parses String with common time format 'timepoint_%d'
	 * and returns a double precision number that represents
	 * the time passed since 1970.
	 */
	public double parseTime_d(String timepoint) {
		String x[] = timepoint.split("timepoint_");
		// Also allow input strings without 'timepoint_' prefix
		String ts = (x.length==1 ? x[0] : x[1]);
		return Double.valueOf(ts.replaceAll("[^0-9.]", ""));
	}
}
