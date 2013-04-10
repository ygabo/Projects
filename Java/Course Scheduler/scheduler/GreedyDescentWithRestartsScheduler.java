package scheduler;

import java.util.Random;
import java.util.Vector;


/**
 * A stub for your Greedy Descent With Restarts scheduler
 */
public class GreedyDescentWithRestartsScheduler extends Scheduler {

	/**
	 * @see scheduler.Scheduler #authorsAndStudentIDs()
	 */
	public String authorsAndStudentIDs() {
		
		return "Yelnil Gabo 70179064 g3d6" ;
	}

	/**
	 * @throws Exception 
	 * @see scheduler.Scheduler#schedule(scheduler.SchedulingInstance)
	 */
	public ScheduleChoice[] solve(SchedulingInstance pInstance) throws Exception {
       
        int currentScore                 = 0 ;
        int greedyScore                  = 0 ;
        int optimalScore                 = Integer.MAX_VALUE ;
        int greedLimit                   = 5 ;
        int searchLimit                  = 20 ;
        Vector<Integer> allPosSchedules  = new Vector<Integer>() ;
		ScheduleChoice[] newSolution ;
		ScheduleChoice[] tempSolution ; 
        for( int i = 0 ; i < pInstance.numRooms ; i++ )
        {
        	for( int j = 0 ; j < pInstance.numTimeslots ; j++ )
        	{
        		allPosSchedules.add( i * 100 + j ) ;
        	}
        }
        
        ScheduleChoice[] optimalSolution = RandomRestart( pInstance, allPosSchedules ) ;
        
		while( !timeIsUp() && evaluator.violatedConstraints( pInstance, optimalSolution ) > 0 )
		{
			
			newSolution  = RandomRestart( pInstance, allPosSchedules ) ;
			tempSolution = newSolution.clone() ;
	        greedyScore  = Integer.MAX_VALUE ;
	        if( greedyScore == 0 ) break ;
	        while( greedLimit > 0 )
	        {
	        	currentScore = greedyScore ;
	        	
	        	// find lower score
	        	while( searchLimit > 0 )
	        	{
	        		tempSolution = Step( pInstance, tempSolution, allPosSchedules ) ;
		        	greedyScore  = violatedConstraints( pInstance, tempSolution ) ;
		        	if( greedyScore == 0 ) break ;
	        		if ( greedyScore <= currentScore && greedyScore > 0 )
	        		{
	        			newSolution = tempSolution ;
	        			break ;
	        		}
	        		
	        		//tempSolution = newSolution.clone() ;
	        		if( searchLimit == 1 ) greedLimit = 0 ;
	        		searchLimit-- ;
	        	}

	        	searchLimit = 20 ;
	        	
		        if( greedyScore <= optimalScore )
	        	{
	        		optimalScore    = greedyScore ;
	        		optimalSolution = newSolution ;
	        	}
		        else if( greedyScore < 0 )
		        {
		        	break ;
		        }
		        else
		        {
		        	greedLimit-- ;
		        }
		        
	        }
	        greedLimit = 5 ;
		}
		return optimalSolution ;
	}

	
    private ScheduleChoice[] RandomRestart( SchedulingInstance pProblem, Vector<Integer> allPossibleSchedules ){

    	int courses                      = pProblem.numCourses ;
        ScheduleChoice[] instance        = new ScheduleChoice[ courses ] ;
        Random r                         = new Random() ;
        Vector<Integer> allPosSchedules  = ( Vector<Integer> ) allPossibleSchedules.clone() ;
        int nextSchedule ;
    	int thisSchedule ;        

        for( int i = 0 ; i < courses ; i++ )
        {
        	nextSchedule = r.nextInt( allPosSchedules.size() ) ;
        	thisSchedule = allPosSchedules.remove( nextSchedule ) ;
        	
        	ScheduleChoice choice = new ScheduleChoice() ;
        	choice.room           = thisSchedule / 100 ;
        	choice.timeslot       = thisSchedule % 100 ;
        	
        	instance[i] = choice ;
        }
        return instance;
    }
    
    private ScheduleChoice[] Step( SchedulingInstance pProblem, ScheduleChoice[] currentInstance, Vector<Integer> allPossibleSchedules ){
    	int courses    = pProblem.numCourses ;
        Random r       = new Random() ;
        int nextSchedule ;
    	int thisSchedule ;
    	int examToChange ;
    	
        Vector<Integer> allPosSchedules = (Vector<Integer>) allPossibleSchedules.clone() ;
        
        for( int i = 0 ; i < currentInstance.length ; i++ )
        {
        	nextSchedule = ( currentInstance[ i ].room * 100 ) + currentInstance[ i ].timeslot ;
        	allPosSchedules.remove( (Integer)nextSchedule ) ;
        }
        
        nextSchedule = r.nextInt( allPosSchedules.size() ) ;
        thisSchedule = allPosSchedules.remove( nextSchedule ) ;

        ScheduleChoice choice = new ScheduleChoice() ;
        choice.room           = thisSchedule / 100 ;
        choice.timeslot       = thisSchedule % 100 ;
        
        examToChange = r.nextInt( courses ) ;
        currentInstance[ examToChange ] = choice ;
        
        return currentInstance;
    }    
    
	public int violatedConstraints(SchedulingInstance pInstance, ScheduleChoice[] pCandidateSchedule) throws Exception {
	
		/* Count student conflicts */
		int conflicts = 0;
		Vector<Vector<Integer>> studentsCourses = pInstance.studentsCourses;
		for (int student = 0; student < pInstance.numStudents; student++) {
			Vector<Integer> coursesOfThisStudent = studentsCourses.elementAt(student);
			for (int i = 0; i < coursesOfThisStudent.size(); i++) {
				for (int j = i+1; j < coursesOfThisStudent.size(); j++) {
					if (pCandidateSchedule[coursesOfThisStudent.elementAt(i)].timeslot == pCandidateSchedule[coursesOfThisStudent.elementAt(j)].timeslot){
						conflicts++;
					}
				}
			}
		}
		return conflicts;
	}

}
