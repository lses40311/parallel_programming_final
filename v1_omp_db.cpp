# include <iostream>
# include <fstream>
# include <string.h>
# include <cstdio>
# include <stdlib.h>
# include <vector>
# include <stdio.h>
# include <sstream>
# include <math.h>
# include <queue>
# include <omp.h>

# define DEBUG 0
# define THREAD_NUMBER 4
# define DIM 2
# define N 8000
# define TAO 10
#define Eps 8
# define MAX_CLASS_NUMBER 30
# define MAX_NEIGHBOR_NUMBER 50

using namespace std ;

int EQ_m[MAX_CLASS_NUMBER][MAX_CLASS_NUMBER] ;
int cluster_number ;

class point {
public:
	vector<float> data ;
	int classID ;
	int type ;
	float operator-(const point &) ;
	point() ;
	void set(string str) ;
} ;

point::point(){	
	classID = -1 ;
	type = 3 ;
}

void point::set(string str){
# if DEBUG
	cout <<"set string: " << str << endl ;	
# endif
	istringstream iss(str);
	for(int i = 0 ; i < DIM ; i++){
		string tok ;
		getline(iss,tok,' ') ;
		data.push_back(atof(tok.c_str())) ;
	}
# if DEBUG
	//cout << "vec_len = " << data.size() << endl ;
# endif
}

float point::operator-(const point & p){
	float sum = 0.0 ;
	for(int i = 0 ; i < DIM ; i++){
		sum += pow((this->data[i] - p.data[i]),2) ;
	}
	return sqrt(sum) ;
}

void init(point * p , ifstream fs){
	p = new point[N] ;
	for(int i = 0 ; i < N ; i++){
		string str ;
		getline(fs,str) ;
		p[i].set(str) ;
	}
}

int find_Neps(point *p , point cur_p , int pos, point **neibors){
//	point x = *cur_p ;
	int Neps = 0 ;
	for(int i = 0 ; i < N ; i++){
		if(pos == i) continue ;
//			cout << "sum = " << cur_p - p[i] << endl ;
		if((cur_p - p[i]) < Eps){
			neibors[Neps] = &p[i] ;
			Neps++ ;
		}
	}
	if(Neps > MAX_NEIGHBOR_NUMBER){
		cout << "neibor number exceed!" << endl ;
		exit(0) ;
	}
	return Neps ;
}

void classify(point *p , point **neibors){
	int i ;
	# pragma omp for private(i)	
	for(i = 0 ; i < N ; i++){
		int Neps = find_Neps(p, p[i] , i , neibors) ;
#if DEBUG
			cout << "find Neps: " << Neps << endl ;
#endif
		if(Neps >= TAO ){ // core object			
			p[i].type = 1 ;
			for(int j = 0 ; j < Neps ; j++){
				if(neibors[j]->type != 1){
					neibors[j]->type = 2 ;
				}
				//cout << "neibor:" <<neibors[j]->data[0] << endl ;
			}
		}
	}
}

int find_neighbors(point * p , point cur_p , int cur_idx , int * neighbor , int current_id){
	int Neps = 0 ;
	for(int i = 0 ; i < N ; i++){
		if(cur_idx == i) continue ;
		if((cur_p - p[i]) < Eps && p[i].type == 1 && p[i].classID == -1){ // near and core
			neighbor[Neps] = i ;
			p[i].classID = current_id ;
			Neps++ ;
		}
	}
	if(Neps > MAX_NEIGHBOR_NUMBER){
		cout << "neibor number exceed!" << endl ;
		exit(0) ;
	}
	return Neps ;
}

void connect_components(point * p , int * neighbor){
	int stop_flag = 0 ;
	int current_id = 1 ;
	while(!stop_flag){
		int start_core ;
		int continue_flag = 0 ;
		int flag = 0 ;
		queue<int> q ;
		// choose core obj randomly
		while(!flag){
			start_core = rand() % N ;
			if(p[start_core].type == 1 && p[start_core].classID == -1){
				flag = 1 ;
			}
		}
//		cout << "start with core: " << start_core << endl ;
		q.push(start_core) ;
		p[start_core].classID = current_id ;
		while(!q.empty()){
			int Neps = 0 ;
			int x = q.front() ;
			q.pop() ;
			Neps = find_neighbors(p,p[x], x , neighbor , current_id) ; //find core neighbor 
			// point*p , current point , current index , int * neighbors
			// put neighbors into queue
			for(int i = 0 ; i < Neps ; i++){				
				q.push(neighbor[i]) ;
			}
		}
		stop_flag = 1 ;
		// check if all core obj has been connect
		for(int i = 0 ; i < N ; i++){
			if(p[i].type == 1 && p[i].classID <= 0){
				stop_flag = 0 ;
				cluster_number = current_id ;
			}
		}
		current_id ++ ;
		if(current_id > MAX_CLASS_NUMBER){
			cout << "exceed MAX class number!" << endl ;
			exit(0) ;
		}
	}
}

int find_neighbor_core(point * p , point cur_p , int cur_idx , int * neighbor){
	int Neps = 0 ;
	for(int i = 0 ; i < N ; i++){
		if(cur_idx == i) continue ;
		if((cur_p - p[i]) < Eps && p[i].type == 1){ // near and core
			neighbor[Neps] = i ;
			Neps++ ;
		}
	}
	if(Neps > MAX_NEIGHBOR_NUMBER){
		cout << "neibor number exceed!" << endl ;
		exit(0) ;
	}
	return Neps ;
}


int knn(point * p , int * neighbor , int n , int * count){
	for(int i = 0 ; i < MAX_CLASS_NUMBER ; i++){
		count[i] = 0 ;
	}
	for(int i = 0 ; i < n ; i++){
		if(p[neighbor[i]].type == 1){
			count[p[neighbor[i]].classID] ++ ;
		}
	}
	int max = -1 ; 
	int max_idx = 0 ;
	for(int i = 0 ; i < MAX_CLASS_NUMBER ; i++){
		if(count[i] > max){
			max = count[i] ;
			max_idx = i ; 
		}
	}
	return max_idx ;
}

void determine_broader( point * p , int * neighbor){
	int * count = new int[MAX_CLASS_NUMBER] ;
	for(int i = 0 ; i < N ; i++){
		if(p[i].type == 2){
			int Neps = 0 ;
			int broader_class = -1 ;
			Neps = find_neighbor_core(p , p[i] , i , neighbor) ;
			broader_class = knn(p , neighbor , Neps , count) ;
			p[i].classID = broader_class ;
		}
	}
	delete[] count ;
}

void output(point * p){
	ofstream output_file ;
	output_file.open("result.txt");
	for(int i = 0 ; i < N ; i++){
		output_file << p[i].type << "," << p[i].classID << endl ; 
	}
	output_file.close() ;
}

int main(){
	ifstream myfile;
	myfile.open ("data_8000.txt");
	string str ;
	// read file
	if( myfile ){
		cout << "open file success" << endl ;
	}
	else{
		cout << "open file failed" << endl ;
	}
	omp_set_num_threads(THREAD_NUMBER) ;
	// equal matrix init
	for(int i = 0 ; i < MAX_CLASS_NUMBER ; i++){
		for(int j = 0 ; j < MAX_CLASS_NUMBER ; j++){
			EQ_m[i][j] = 0 ;
		}
	}
	point * p = new point[N] ;
	for(int i = 0 ; i < N ; i++){
		string str ;
		getline(myfile,str) ;
		p[i].set(str) ;
	}
	myfile.close() ;
	
		
#pragma omp parallel
{
	point * neibors[MAX_NEIGHBOR_NUMBER] ;
	classify(p, neibors) ;
}

//# pragma omp parallel
//{
	int * neighbor = new int[MAX_NEIGHBOR_NUMBER] ;
	connect_components(p,neighbor) ; // for core obj
	delete[] neighbor ;
//}

	int * neighbor = new int[MAX_NEIGHBOR_NUMBER] ;
	determine_broader(p,neighbor) ; // for broader obj

	output(p) ;

# if DEBUG
	
# endif

	return 0 ;
}
