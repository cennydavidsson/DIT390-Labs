import TSim.*;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Semaphore;
import java.util.logging.Level;
import java.util.logging.Logger;

public class Lab1 {
    
    public static void main(String[] args)  throws InterruptedException{
        new Lab1(args);
    }
    
    public Lab1(String[] args) throws InterruptedException{

        int trainSpeed1 = (int) (args.length >= 1 ? Integer.parseInt(args[0]) : Math.ceil(Math.random()*17));
        int trainSpeed2 = (int) (args.length >= 2 ? Integer.parseInt(args[1]) : Math.ceil(Math.random()*17));
        int simSpeed    = args.length        >= 3 ? Integer.parseInt(args[2]) : 100;
        
        List<Semaphore> semList = new ArrayList<>();
        for (int i = 0; i < 6; i++) {
            semList.add(new Semaphore(1));
        }
        
        TrainThread t1 = new TrainThread(1, trainSpeed1, (ArrayList<Semaphore>) semList, simSpeed);
        new Thread(t1).start();
        
        TrainThread t2 = new TrainThread(2, trainSpeed2, (ArrayList<Semaphore>) semList, simSpeed);
        new Thread(t2).start();   
    }
}

class TrainThread implements Runnable {
    
    public enum Direction { 
        up, down 
    }
    
    int id, speed, speedparameter;
    Semaphore semTrack1, semTrack3, semCrossing, semMiddleHigh, semSensor6, semSensor9;
    TSimInterface tsi = TSimInterface.getInstance();
    Boolean defaultTrack = true;
    
    TrainThread(int trainID, int startspeed, ArrayList<Semaphore> semList, int speedparameter) throws InterruptedException {
        id                  = trainID;
        speed               = startspeed;
        this.speedparameter = speedparameter;

        semTrack1           = semList.get(0);
        semTrack3           = semList.get(1);
        semCrossing         = semList.get(2);
        semMiddleHigh       = semList.get(3);
        semSensor6          = semList.get(4);
        semSensor9          = semList.get(5);
        
        (id == 1 ? semTrack1 : semTrack3).acquire();
    }
    
    @Override
    public void run() {
        try {
            runTrain();
        } catch (InterruptedException ex) {
            Logger.getLogger(TrainThread.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
    
    public void runTrain() throws InterruptedException {
        
        try {
            tsi.setDebug(true);
            tsi.setSpeed(id, speed);
                      
            while(true) {
                SensorEvent sensorEvent = tsi.getSensor(id);
                
                if (id == sensorEvent.getTrainId()) {
                    int activeSensor = getActiveSensorFor(sensorEvent);
                    switch (activeSensor) {
                        case 1:
                            if (goingUp(id)) {
                                changeDirection(id);
                            }
                            break;
                            
                        case 2:
                            if (goingDown(id)) {
                                if (!semCrossing.tryAcquire()) {
                                    getSemCrossing(id, speed);
                                }
                            }
                            if (goingUp(id)) {
                                semCrossing.release();
                            }
                            break;
                            
                        case 3:
                            if (goingUp(id)) {
                                semCrossing.release();
                                changeDirection(id);
                            }
                            
                            if (goingDown(id)) {
                                if (!semCrossing.tryAcquire()) {
                                    getSemCrossing(id, speed);
                                }
                            }
                            break;
                            
                        case 4: case 5:
                            if (goingDown(id)) {
                                semCrossing.release();
                                acquireSemWithWait(semSensor6);
                                setSwitch(1, activeSensor == 4 ? Direction.up : Direction.down);
                            }
                            if (goingUp(id)) {
                                semSensor6.release();
                                if (!semCrossing.tryAcquire()) {
                                    getSemCrossing(id, speed);
                                }
                            }
                            break;
                            
                        case 6:
                            if (goingDown(id)) {
                               tryReleaseAndAcquireSemsAtSwitch(semTrack1, semMiddleHigh, 3);                                
                            }
                            if (goingUp(id)) {
                               tryReleaseAndAcquireSemsAtSwitch(semMiddleHigh, semTrack1, 1);                                 
                            }
                            break;
                            
                        case 7: case 10:
                            if (goingDown(id)) {
                                acquireSemWithWait(semSensor9);
                                setSwitch(2, activeSensor == 7 ? Direction.up : Direction.down);
                            }
                            if (goingUp(id)) {
                                semSensor9.release();
                            }
                            break;
                            
                        case 8: case 11:
                            if (goingDown(id)) {
                                semSensor6.release();
                            }
                            if (goingUp(id)) {
                                acquireSemWithWait(semSensor6);
                                setSwitch(3, activeSensor == 8 ? Direction.up : Direction.down);
                            }
                            break;
                            
                        case 9:
                            if (goingUp(id)) {
                                tryReleaseAndAcquireSemsAtSwitch(semTrack3, semMiddleHigh, 2);
                            }
                            if (goingDown(id)) {
                                tryReleaseAndAcquireSemsAtSwitch(semMiddleHigh, semTrack3, 4);
                            }
                            break;
                            
                        case 12: case 14:
                            if (goingDown(id))  {
                                semSensor9.release();
                            }
                            if (goingUp(id)) {
                                acquireSemWithWait(semSensor9);
                                setSwitch(4, activeSensor == 12 ? Direction.up : Direction.down);
                            }
                            break;
                            
                        case 13: case 15:
                            if (goingDown(id)) {
                                changeDirection(id);
                            }
                            break;
     
                        default: break;
                    }
                }
            }
        }
        catch (CommandException e) {
            e.printStackTrace();    // or only e.getMessage() for the error
            System.exit(1);
        }
    }
    
    private int getActiveSensorFor(SensorEvent sensorEvent) {
        
        int x = sensorEvent.getXpos();
        int y = sensorEvent.getYpos();
        
        if ( sensorEvent.getStatus() == 1 ) {
            if (x == 14 && y ==  3 ) { return  1; }
            if (x ==  6 && y ==  5 ) { return  2; }
            if (x == 14 && y ==  5 ) { return  3; }
            if (x == 14 && y ==  7 ) { return  4; }
            if (x == 14 && y ==  8 ) { return  5; }
            if (x == 19 && y ==  8 ) { return  6; }
            if (x ==  6 && y ==  9 ) { return  7; }
            if (x == 13 && y ==  9 ) { return  8; }
            if (x ==  1 && y == 10 ) { return  9; }
            if (x ==  6 && y == 10 ) { return 10; }
            if (x == 13 && y == 10 ) { return 11; }
            if (x ==  5 && y == 11 ) { return 12; }
            if (x == 14 && y == 11 ) { return 13; }
            if (x ==  5 && y == 13 ) { return 14; }
            if (x == 14 && y == 13 ) { return 15; }
        }
        return 0;
    }
    
    private void setSwitch(int trackSwitch, Direction dir) throws CommandException {
        switch (trackSwitch) {
            case 1:
                tsi.setSwitch(17, 7, dir == Direction.up ? 2 : 1);
                break;
        
            case 2:
                tsi.setSwitch(4, 9, dir == Direction.up ? 1 : 2);
                break;
                
            case 3:
                tsi.setSwitch(15, 9, dir == Direction.up ? 2 : 1);
                break;
                
            case 4:
                tsi.setSwitch(3, 11, dir == Direction.up ? 1 : 2);
                break;
            
            default: break;
        }
    }
    
    private void changeDirection(int i) throws CommandException, InterruptedException {
        tsi.setSpeed(i, 0);
        speed *= -1;
        Thread.sleep(1000+2*speedparameter*Math.abs(speed));
        tsi.setSpeed(i, speed);
    }
    
    private Boolean goingUp(int trainID) {
        return (trainID == 1 && speed < 0) || (trainID == 2 && speed > 0);
    }
    
    private Boolean goingDown(int trainID) {
        return (trainID == 1 && speed > 0) || (trainID == 2 && speed < 0);
    }
    
    private void getSemCrossing(int id, int speed) throws CommandException {
        Boolean canAcquire = false;
        tsi.setSpeed(id,0);
        while (!canAcquire) {
            canAcquire = semCrossing.tryAcquire();
        }        
        tsi.setSpeed(id,speed);
    }

    private void acquireSemWithWait(Semaphore sem) throws CommandException, InterruptedException {
        Boolean canAcquire = false;
        if (!sem.tryAcquire()) {
            tsi.setSpeed(id, 0);
            while (!canAcquire) {
                canAcquire = sem.tryAcquire();
            }
            tsi.setSpeed(id, speed);
        }
    }

    private void tryReleaseAndAcquireSemsAtSwitch(Semaphore releaseSem, Semaphore acquireSem, int railsSwitch) throws CommandException, InterruptedException {
        if (defaultTrack) {
            releaseSem.release();
            defaultTrack = false;
        }
        if (acquireSem.tryAcquire()) {
            setSwitch(railsSwitch, Direction.up);
            defaultTrack = true;            
        } else {
            setSwitch(railsSwitch, Direction.down);
        }
    }
}