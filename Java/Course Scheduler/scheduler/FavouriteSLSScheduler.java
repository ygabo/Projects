package scheduler;

import java.util.Random;
import java.util.Vector;


/**
 * A stub for your second scheduler
 */
public class FavouriteSLSScheduler extends Scheduler {

	/**
	 * @see scheduler.Scheduler#authorsAndStudentIDs()
	 */
	public String authorsAndStudentIDs() {
		return "Yelnil Gabo 70179064 g3d6";
	}

	/**
	 * @throws Exception 
	 * @see scheduler.Scheduler#schedule(scheduler.SchedulingInstance)
	 */
	public ScheduleChoice[] solve(SchedulingInstance pInstance) throws Exception {

		Vector<Integer> allPosSchedules  = new Vector<Integer>() ;
		int newScore                     = 0 ;
		int currentScore                 = 0 ;
		ScheduleChoice[] currentSolution ; 
		ScheduleChoice[] newSolution ;
		ScheduleChoice[] solutionOptimal ;
		
        for( int i = 0 ; i < pInstance.numRooms ; i++ )
        {
        	for( int j = 0 ; j < pInstance.numTimeslots ; j++ )
        	{
        		allPosSchedules.add( i * 100 + j ) ;
        	}
        }
        
		currentSolution = RandomRestart( pInstance, allPosSchedules ) ;
        currentScore    = evaluator.violatedConstraints( pInstance, currentSolution ) ;
        solutionOptimal = currentSolution ;
        
		while( !timeIsUp() && evaluator.violatedConstraints(pInstance, solutionOptimal ) > 0 ){
			
			newSolution  = RandomRestart( pInstance, allPosSchedules ) ;
			newScore = evaluator.violatedConstraints( pInstance, currentSolution ) ;
			
			if ( newScore <= currentScore )
			{
				currentScore = newScore ;
				solutionOptimal = newSolution ;
			}			
		}
		return solutionOptimal;
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
}