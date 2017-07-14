
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
  	// 	highlighted = new ConcurrentHashMap<String, float[]>(8, 0.9f, 1);
  	// 	trajectories = new HashMap<String, List<String>>();
  	// //	agentSkeletons = new HashMap<String, Skeleton>();
  	 }

        @Override
	public void onStart(final ConnectedNode connectedNode) {
	    node = connectedNode;
	    System.out.println("Start node");
	    //   final Publisher<visualization_msgs.MarkerArray> publisher =
	    pub = connectedNode.newPublisher("/visualization_marker_array",visualization_msgs.MarkerArray._TYPE);
	    //    markers =  new ConcurrentHashMap<String, Marker>(8, 0.9f, 1);
	   
	    //  pub = publisher;
	    // connectedNode.executeCancellableLoop(new CancellableLoop() {
	    // 	    private int sequenceNumber;
		    
	    // 	    @Override
	    // 	    protected void setup() {
	    // 		sequenceNumber = 0;
	    // 	    }
		    
	    // 	    @Override
	    // 	    protected void loop() throws InterruptedException {
	    // 		MarkerArray arr = pub.newMessage();
	    // 		for(Marker mrk : markers.values()) {
	    // 		    arr.getMarkers().add(mrk);
	    // 		}
	    // 		pub.publish(arr);
	    // 		markers.clear();
	    // 		sequenceNumber++;
	    // 		Thread.sleep(1000);
	    // 	    }
	    // 	});

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
		    System.out.println("test inside publish Marker");
			// wait for node to be ready
		    synchronized (markers) {
			MarkerArray arr = pub.newMessage();
			
			for(Marker mrk : markers.values()) {
			    arr.getMarkers().add(mrk);
			}
			pub.publish(arr);
			System.out.println("pub");
			//	System.out.println(markers.values());
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
	System.out.println("add_arrow");
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
	System.out.println("HALLO5");
	m.getColor().setR(1);
	m.getColor().setG(0);
	m.getColor().setB(0);
	m.getColor().setA(1.0f);
	System.out.println("HALLO1");
	        
  	 	 //add marker to map
	final StringBuilder identifier = new StringBuilder();
	identifier.append(m.getNs()).append('_').append(m.getId());
	System.out.println("testwewew");
	System.out.println(m.getId());
	System.out.println(identifier.toString());
  	 	 synchronized(markers) {
  	 	     markers.put(identifier.toString(),m);
System.out.println("tessssasdsadsasstwewew");
  	 	 }
		 	System.out.println("tesssssstwewew");
  	 	 synchronized(markersCache) {
  	 	     markersCache.put(identifier.toString(),m);
System.out.println("tesssssadasdsasstwewew");
  	 	 }
		 System.out.println("test123");
					
  	 	 publishMarkers();
		 if (pub == null)
		     System.out.println("WOW");
		 else
		     System.out.println("NOOOIN");

		 return "Cool that worked";
  	 }

    public String removeMapObject(String A){
	System.out.println("I removed it> ");
	return "checked";
    }

       

	public Marker createMarker() {
	    System.out.println("createMarker");
	    waitForNode();
		System.out.println("node");
		Marker m = node.getTopicMessageFactory().newFromType(visualization_msgs.Marker._TYPE);
		System.out.println("node2");
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
	
	 System.out.println(pub);
	 System.out.println(node);
		// wait for node to be ready
		try {
			while(node == null || pub == null) {
				Thread.sleep(200);
			}
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
     }

    public String detectedObject(float[] pose){
		final Marker m;
	System.out.println("add_arrow");
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
	System.out.println("HALLO5");
	m.getColor().setR(1);
	m.getColor().setG(0);
	m.getColor().setB(0);
	m.getColor().setA(1.0f);
	System.out.println("HALLO1");
	        
  	 	 //add marker to map
	final StringBuilder identifier = new StringBuilder();
	identifier.append(m.getNs()).append('_').append(m.getId());
	System.out.println("testwewew");
	System.out.println(m.getId());
	System.out.println(identifier.toString());
  	 	 synchronized(markers) {
  	 	     markers.put(identifier.toString(),m);
System.out.println("tessssasdsadsasstwewew");
  	 	 }
		 	System.out.println("tesssssstwewew");
  	 	 synchronized(markersCache) {
  	 	     markersCache.put(identifier.toString(),m);
System.out.println("tesssssadasdsasstwewew");
  	 	 }
		 System.out.println("test123");
					
  	 	 publishMarkers();

		 System.out.println("COOL THAT WORKED TOO");
		 return "Cool that worked2";
	
    }

}
